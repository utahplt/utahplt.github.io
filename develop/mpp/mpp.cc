/*
    mpp.c -- Defun preprocessor.
*/
/*
    Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
    Copyright (c) 1990, Giuseppe Attardi.

    ECoLisp is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.

    1999 Modified by Gregory Lampshire for mzscheme.

*/

//  $Id: mpp.cc,v 1.4 2000/06/05 01:17:48 gbol Exp gbol $

/*
	Usage:
		See mpp -h

	The file named file.d is preprocessed and the output will be
	written to the file whose name is file.cc.


	The function definition:

	@(defun name ({var}*
		      [&optional {var | (var [initform [svar]])}*]
		      [&rest var]
		      [&key {var |
			     ({var | (keyword var)} [initform [svar]])}*
			    [&allow_other_keys]]
		      [&aux {var | (var [initform])}*])

		C-declaration

	@

		C-body

	@)

	name can be either an identifier or a full C procedure header
	enclosed in quotes (').

	&optional may be abbreviated as &o.
	&rest may be abbreviated as &r.
	&key may be abbreviated as &k.
	&allow_other_keys may be abbreviated as &aok.
	&aux may be abbreviated as &a.

	Each variable becomes a C variable.

	Each supplied-p parameter becomes a boolean C variable.

	Initforms are C expressions.
	It an expression contain non-alphanumeric characters,
	it should be surrounded by backquotes (`).


	Function return:

		@(return {form}*)


        Gregory:

        The following forms are also recognized:

        @(defvar varname)
        @(make-keyword keyword-name)               
        @(defconstant varname value)

        A proc instead of defun implies the creation of MLT specific objects.
        @returns are not supported I do not know what happens in the code if they are used.

*/

#if defined(__CYGWIN32__) || defined(__MINGW32__)
#define unix
#endif

#define objstr "Scheme_Object*"
#define Cnil "scheme_null"

#include <fstream.h>
#include <stdiostream.h>
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <string>
#include <cstdlib>
#include <getopt.h>
#include <stl.h>


#define POOLSIZE        2048
#define MAXREQ          16
#define MAXOPT          16
#define MAXKEY          16
#define MAXAUX          16
#define MAXRES          16

#define TRUE            1
#define FALSE           0

FILE *in, *out;

ostdiostream *fout;
string filestem;

void put_siglist(void);
void put_klist(void);
void put_defvarlist(void);
void put_defconstantlist(void);
string make_cfunction_name(const char *src);

bool is_proc;  //  whether declaration is a simple defun or a proc


static const string allowed_symbol_chars = "_-!<>?*$";
static const int slength = allowed_symbol_chars.length();


char filename[BUFSIZ];
int lineno;
int tab;
int tab_save;

char pool[POOLSIZE];
char *poolp;

char *function;          //  function name
string cfunction;        //  'function' cleaned for C-usage

class funcsig
{
public:
  int nreq;              //  number of required arguments
  string cfunction;      //  C-version, with 'L' appended
  string function;       //  scheme function name as from defun

  funcsig(const int n, const string& cf, const string& f) :
    nreq(n), cfunction(cf), function(f) { }
};

list<funcsig> siglist;    //  list of function signatures

class keywordsym
{
public:
  string name;
  keywordsym(const string n) : name(n) { }
  void display(ostdiostream& str)
  {
    string new_name = make_cfunction_name(string(string("K") + name).c_str());
    string k_name = ":" + name;
    str << "\t" << new_name.c_str() << " = make_keyword(env, &";
    str << new_name.c_str() << ", ";
    str << "\"" << k_name.c_str() << "\");\n";
  }
};

list<keywordsym> klist;


class defvar
{
public:
  string name, value;
  defvar(const string n, const string v = "") : name(n), value(v) { }
  void display(ostdiostream& str)
  {
    const string cname = make_cfunction_name(string(string("S")+name).c_str());
    str << '\t' << cname << " = make_ordinary(env, &" << cname << ", \"" << name << "\");\n";
  }
};

list<defvar> defvarlist;


class defconstant
{
public:
  const string name, value;
  defconstant(const string n, const string v) : name(n), value(v) { }
  void display(ostdiostream& str)
  {
    str << '\t' << "scheme_add_global_constant(\"" << name << "\", " <<
        value << ", env);\n";
  }
};

list<defconstant> defconstantlist;


string make_cfunction_name(const char *src)
{
  string result = "";
  if(src == 0) { return result; }
  char *p = (char*)src;
  while(*p != '\0')
    {
      if(isalpha(*p) || isdigit(*p) || *p == '_') { result += *p; }
      else if(*p == '-') { result += '_'; }
      //  if it's a wierd char, just do the underscore thing
      else { result += '_'; }
      p++;
    }
  return result;
}



void put_line_def()
{ *fout << "\n#line " << lineno << "\" " << filename << "\"\n"; }



static string prefix = "__";
static string postfix = "__";


string make_cspecial_name(const char *src)
{
  string tmp = make_cfunction_name(src);
  string result = prefix + tmp + postfix;
  return result;
}


char *required[MAXREQ];  //  required arguments
int nreq;                //  number of required 

string req(const int index) { return string(required[index]); }
string creq(const int index) { return make_cfunction_name(required[index]); }
string sreq(const int index) { return make_cspecial_name(required[index]); }

struct optional {
public:
  char *o_var;           //  optional variable name
  string o_var_c;        //  c-version of name
  char *o_init;          //  init value for optional value
  char *o_svar;          //  supplied-p parameter variable name
  string o_svar_c;      //  c-version of supplied-p parameter

public:
  string opt() { return string(o_var); }
  string copt() { return make_cfunction_name(o_var); }
  string sopt() { return make_cspecial_name(o_var); }

} optional[MAXOPT];

int nopt;                //  number of optional arguments

bool rest_flag;
char *rest_var;

bool key_flag;
struct keyword {
public:
  char *k_key;
  string k_key_c;        //  c-version of keyword name
  char *k_var;           //  keyword variable
  string k_var_c;
  char *k_init;
  char *k_svar;
  string k_svar_c;       //  c-version of supplied-p parameter

public:
  string key_orig() { return string(k_key); }
  string ckey() { return make_cfunction_name(key_orig().c_str()); }
  string skey() { return string(make_cspecial_name(key_orig().c_str())); }

} keyword[MAXKEY];

int nkey;                //  number of key arguments
bool allow_other_keys_flag;

struct aux {
  char *a_var;
  char *a_init;
} aux[MAXAUX];
int naux;                //  number of aux arguments

char *result[MAXRES];
int nres;                //  number of results


static int already_put_tail = 0;  //  records wether tail for module has been written



void error(char *s)
{
  cerr << "Error in line " << lineno << ": " << s << "." << endl;
  exit(-1);
}

int readc()
{
  int c;

  c = getc(in);
  if (feof(in)) {
    if (function != NULL) error("unexpected end of file");
    if(!already_put_tail) { put_siglist(); }
    exit(0);
  }
  if (c == '\n') {
    lineno++;
    tab = 0;
  } else if (c == '\t')
    tab++;
  return(c);
}


int nextc()
{
  int c;

  while (isspace(c = readc()))
    ;
  return(c);
}

void unreadc(int c)
{
  if (c == '\n')
    --lineno;
  else if (c == '\t')
    --tab;
  ungetc(c, in);
}

void put_tabs(int n)
{
  int i;

  for (i = 0;  i < n;  i++)
    putc('\t', out);
}

void pushc(int c)
{
  if (poolp >= &pool[POOLSIZE])
    error("buffer pool overflow");
  *poolp++ = c;
}

inline void eat_optional_semicolon()
{
  int c;
  if((c=readc()) == ';') { /* do nothing */ }
  else { unreadc(c); }
}


char *
read_token()
{
  int c;
  char *p;

  p = poolp;
  if ((c = nextc()) == '`') {
    while ((c = readc()) != '`')
      pushc(c);
  } else {
    do { pushc(c); } while (isalnum(c = readc()) 
                            || (allowed_symbol_chars.find(c)<slength));
    unreadc(c);
  }
  pushc('\0');
  return(p);
}

void reset()
{
  int i;

  poolp = pool;
  function = NULL;
  cfunction = "";
  nreq = 0;
  for (i = 0;  i < MAXREQ;  i++) { required[i] = NULL; }
  nopt = 0;
  for (i = 0;  i < MAXOPT;  i++) {
    optional[i].o_var
      = optional[i].o_init
      = optional[i].o_svar
      = NULL;
    optional[i].o_var_c = "";
    optional[i].o_svar_c = "";
  }
  rest_flag = FALSE;
  rest_var = "ARGS";
  key_flag = FALSE;
  nkey = 0;
  for (i = 0;  i < MAXKEY;  i++) {
    keyword[i].k_key
      = keyword[i].k_var
      = keyword[i].k_init
      = keyword[i].k_svar
      = NULL;
    keyword[i].k_key_c = "";
    keyword[i].k_var_c = "";
    keyword[i].k_svar_c = "";
  }
  allow_other_keys_flag = FALSE;
  naux = 0;
  for (i = 0;  i < MAXAUX;  i++) {
    aux[i].a_var
      = aux[i].a_init
      = NULL;
  }
}

void get_function()
{
  function = read_token();
  cfunction  = make_cfunction_name(function);
}

void get_lambda_list()
{
  int c;
  char *p;

  if ((c = nextc()) != '(')
    error("( expected");
  for (;;) {
    if ((c = nextc()) == ')')
      return;
    if (c == '&') {
      p = read_token();
      goto OPTIONAL;
    }
    unreadc(c);
    p = read_token();
    if (nreq >= MAXREQ)
      error("too many required variables");
    required[nreq++] = p;
  }

 OPTIONAL:
  if (strcmp(p, "optional") != 0 && strcmp(p, "o") != 0)
    goto REST;
  for (;;  nopt++) {
    if ((c = nextc()) == ')')
      return;
    if (c == '&') {
      p = read_token();
      goto REST;
    }
    if (nopt >= MAXOPT)
      error("too many optional argument");
    if (c == '(') {
      optional[nopt].o_var = read_token();
      optional[nopt].o_var_c = make_cspecial_name(optional[nopt].o_var);
      if ((c = nextc()) == ')')
        continue;
      unreadc(c);
      optional[nopt].o_init = read_token();
      if ((c = nextc()) == ')') continue;
      unreadc(c);
      optional[nopt].o_svar = read_token();
      optional[nopt].o_svar_c = make_cspecial_name(optional[nopt].o_svar);
      if (nextc() != ')') error(") expected");
    } else {
      unreadc(c);
      optional[nopt].o_var = read_token();
      optional[nopt].o_var_c = make_cfunction_name(optional[nopt].o_svar);
    }
  }

 REST:
  if (strcmp(p, "rest") != 0 && strcmp(p, "r") != 0)
    goto KEYWORD;
  rest_flag = TRUE;
  if ((c = nextc()) == ')' || c == '&') { error("&rest var missing"); }
  unreadc(c);
  rest_var = read_token();
  if ((c = nextc()) == ')')
    return;
  if (c != '&')
    error("& expected");
  p = read_token();
  goto KEYWORD;

 KEYWORD:
  if (strcmp(p, "key") != 0 && strcmp(p, "k") != 0)
    goto AUX;
  key_flag = TRUE;
  for (;;  nkey++) {
    if ((c = nextc()) == ')')
      return;
    if (c == '&') {
      p = read_token();
      if (strcmp(p, "allow_other_keys") == 0 ||
          strcmp(p, "aok") == 0) {
        allow_other_keys_flag = TRUE;
        if ((c = nextc()) == ')')
          return;
        if (c != '&')
          error("& expected");
        p = read_token();
      }
      goto AUX;
    }
    if (nkey >= MAXKEY)
      error("too many optional argument");
    if (c == '(') {
      if ((c = nextc()) == '(') {
        p = read_token();
        if (p[0] != ':' || p[1] == '\0')
          error("keyword expected");
        keyword[nkey].k_key = p + 1;
        keyword[nkey].k_key_c = make_cspecial_name(keyword[nkey].k_key);
        keyword[nkey].k_var = read_token();
        keyword[nkey].k_var_c = make_cspecial_name(keyword[nkey].k_var);
        if (nextc() != ')')
          error(") expected");
      } else {
        unreadc(c);
        keyword[nkey].k_key = keyword[nkey].k_var = read_token();
        keyword[nkey].k_key_c = keyword[nkey].k_var_c = make_cspecial_name(keyword[nkey].k_key);
      }
      if ((c = nextc()) == ')')
        continue;
      unreadc(c);
      keyword[nkey].k_init = read_token();
      if ((c = nextc()) == ')')
        continue;
      unreadc(c);
      keyword[nkey].k_svar = read_token();
      keyword[nkey].k_svar_c = make_cspecial_name(keyword[nkey].k_svar);
      if (nextc() != ')')
        error(") expected");
    } else {
      unreadc(c);
      keyword[nkey].k_key = keyword[nkey].k_var = read_token();
      keyword[nkey].k_key_c = keyword[nkey].k_var_c = make_cspecial_name(keyword[nkey].k_key);
    }
  }

 AUX:
  if (strcmp(p, "aux") != 0 && strcmp(p, "a") != 0)
    error("illegal lambda-list keyword");
  for (;;) {
    if ((c = nextc()) == ')')
      return;
    if (c == '&')
      error("illegal lambda-list keyword");
    if (naux >= MAXAUX)
      error("too many auxiliary variable");
    if (c == '(') {
      aux[naux].a_var = read_token();
      if ((c = nextc()) == ')')
        continue;
      unreadc(c);
      aux[naux].a_init = read_token();
      if (nextc() != ')')
        error(") expected");
    } else {
      unreadc(c);
      aux[naux].a_var = read_token();
    }
    naux++;
  }
}

void get_return()
{
  int c;

  nres = 0;
  for (;;) {
    if ((c = nextc()) == ')')
      return;
    unreadc(c);
    result[nres++] = read_token();
  }
}



void put_siglist(void)
{
  *fout << "Scheme_Object *init_" << filestem << "(Scheme_Env *env)\n";
  *fout << "{\n";
  list<funcsig>::iterator iter = siglist.begin();
  while(iter != siglist.end())
    {
      *fout << "\tscheme_add_global_constant(\"" << (*iter).cfunction << "\",\n";
      *fout << "\t\tscheme_make_prim_w_arity(L" << (*iter).function << ", \""
            << (*iter).cfunction << "\", "
            << (*iter).nreq 
            << ", " << -1 << "),\n";
      *fout << "\t\tenv);\n";
      iter++;
    }
  *fout << "\n\n";
  put_klist();
  put_defvarlist();
  put_defconstantlist();
  *fout << "\n\n";
  *fout << "\treturn scheme_true;\n";
  *fout << "}\n";
}


void put_defvarlist(void)
{
  list<defvar>::iterator iter = defvarlist.begin();
  while(iter != defvarlist.end())
    {
      (*iter).display(*fout);
      iter++;
    }
}


void put_defconstantlist(void)
{
  list<defconstant>::iterator iter = defconstantlist.begin();
  while(iter != defconstantlist.end())
    {
      (*iter).display(*fout);
      iter++;
    }
}


void put_klist(void)
{
  list<keywordsym>::iterator iter = klist.begin();
  while(iter != klist.end())
    {
      (*iter).display(*fout);
      iter++;
    }
}



void put_init(void)
{
#if 0
  *fout << "Scheme_Object *L" << cfunction << "(int argc, Scheme_Object **argv);\n";
  *fout << "Scheme_Object *init_" << cfunction << "_function(Scheme_Env *env)\n";
  *fout << "{\n";
  *fout << "\tscheme_add_global_constant(\"" << function << "\",\n";
  *fout << "\t\tscheme_make_prim_w_arity(L" << cfunction << ", \"" << function << "\", " << nreq 
        << ", " << -1 << "),\n";
  *fout << "\t\tenv);\n";
  *fout << "\treturn scheme_true;\n";
  *fout << "}\n\n";
#endif
}



void put_fhead()
{
  bool b = FALSE; char *p = function;
  int i;
#if 0
  fputc('L', out);
  while (!b && *p != '\0')
    {
      fputc(*p, out);
      b = (*p++ == '(');
    }
#endif

  *fout << "Scheme_Object *L" << cfunction;
  fprintf(out, "(int argc, Scheme_Object **argv)\n");

#if 0
  if (b) {
	/*
	@(defun `assoc_or_rassoc(object (*car_or_cdr)())`
	     (item a_list &key test test_not key)
	must become:
	Lassoc_or_rassoc(int narg, object (*car_or_cdr)(),
	      object item, object a_list, ...)
	*/
	fprintf(out, "int narg, ");
	while (*p != ')' || p[1] != '\0')
		fputc(*p++, out);
	}
  else { fprintf(out, "(int narg", function); }
	
  for (i = 0; i < nreq; i++) { fprintf(out, ", object %s", required[i]); }
  if (nopt > 0 || rest_flag || key_flag) { fprintf(out, ", ..."); }
  fprintf(out, ")\n");
  if (b) {
	while (*p++ != ')') ;
	fprintf(out, "%s", p);  /* declaration of extra first arg */
	}
#endif

  fprintf(out, "{");
}



void put_declaration()
{
  int i;

  //  output special process objects
  if(is_proc)
    {
      *fout << "\tProcess_Environment pe(\"" << function << "\");\n";
      *fout << "\tpe.start();\n";
    }

  //  output temp macros
  *fout << "\n#define defun_assert(cond, msg)\\\n";
  *fout << "\tif(!(cond)) { scheme_signal_error(\"" << function << ": %s\", (char*)(msg)); }\n";
  *fout << "\n#define defun_assert_cpp(cond, msg)\\\n";
  *fout << "\tif(!(cond)) { scheme_signal_error(\"" << function << ": %s\", (msg).c_str()); }\n";
  put_line_def();

  *fout << "#define defun_assert_args(cond, args)\\\n";
  *fout << "\tif(!(cond)) { scheme_signal_error(\"" << function << ": \" args); }\n";
  put_line_def();

  *fout << "#define check_type(x, t, m) \\\n";
  *fout << "\tif(SCHEME_TYPE(x)!=t)\\\n\t{ scheme_signal_error";
  *fout << "(\"wrong type for %s in function %s, expected %s\", SCHEME_SYM_VAL(x), \""
        << function << "\", m); }\n\n";

  //  Check # arguments.
  fprintf(out, "\tif (argc < %d) scheme_signal_error(\"not enough arguments to function %s\");\n",
          nreq, function);
  if (nopt > 0 && !rest_flag && !key_flag)
    fprintf(out, "\tif (argc > %d) scheme_signal_error(\"too many arguments to function %s\");\n",
            nreq + nopt, function);

  //  output handy references
  put_line_def();
  *fout << "\tconst string ___func_name___ = \"" << function << "\";\n";

  //
  //  handle required arguments
  //
  *fout << "\t//  # required arguments: " << nreq << '\n';
  for(i=0; i<nreq; i++)
    { *fout << "\tScheme_Object* " << sreq(i) << " = argv[" << i << "];\n"; }
  if(nreq == 0) { *fout << "\t//  ... no required args ...\n"; }


  //
  //  handle optional argumens
  //
  *fout << "\t//  # optional arguments: " << nopt << '\n';
  for (i = 0;  i < nopt;  i++) { *fout << "\tScheme_Object* " << optional[i].sopt() << ";\n"; }
  for (i = 0;  i < nopt;  i++) {
    if (optional[i].o_svar != NULL)
      { *fout << "\tbool " << optional[i].o_svar_c << ";\n"; }
  }
  if(nopt == 0) { *fout << "\t//  ... no optional args ...\n"; }

  if(rest_flag)
    {
      //  a &rest parameter was specified
      *fout << "\t//  a &rest option was specified\n";
      *fout << "\tScheme_Object *__" << rest_var << "__;\n";
    }
  else
    { *fout << "\t//  ... no rest arg ...\n"; }


  //
  //  handle keyword arguments
  //
  put_line_def();
  *fout << "\t//  # keyword arguments: " << nkey << '\n';
  if (nkey > 0) { fprintf(out, "\t%s KEYS[%d];\n", objstr, nkey); }

  for (i = 0;  i < nkey;  i++)
    { *fout << "#define " << keyword[i].skey() << "\tKEY_VARS[" << i << "]\n"; }

  for (i = 0;  i < nkey;  i++) {
    if (keyword[i].k_svar != NULL) {
      //fprintf(out, "#define %s bool(KEY_VARS[%d])\n", keyword[i].k_svar_c.c_str(), nkey+i);
      fprintf(out, "\tbool %s;\n", keyword[i].k_svar_c.c_str());
    }
  }

  //
  //  handle aux arguments
  //
  put_line_def();
  for (i = 0;  i < naux;  i++)
    fprintf(out, "\t%s %s;\n", objstr, aux[i].a_var);

  if (nopt == 0 && !rest_flag && !key_flag)
    {
      //  in mzscheme, the required number of options is specified in the function initializer
      //  fprintf(out, "\tcheck_arg(%d);\n", nreq);
    }
  else
    {
      //
      //  Keyword args are stored in first nkey part of the array as the symbol,
      //  the upper portion of the array stores supplied-p parameters, if any.
      //
      if (key_flag) { fprintf(out, "\t%s KEY_VARS[%d];\n", objstr, 2*nkey); }


      // 
      //  If there are more args then what is required, then these must first be bound
      //  to the optional args.  Here we need to check that after the required arguments
      //  and if there are any arguments left and if the remaining arguments are not
      //  keyword arguments, then they must be assigned to the optional arguments.
      //  If assigned to the optional arguments, then the &rest parameter should not
      //  pick up the optional arguments.
      //
      put_line_def();
      for (i = 0;  i < nopt;  i++) {
        *fout << "\tif (argc > " << nreq+i << ") {\n";
        *fout << "\t\t" << optional[i].sopt() << " = argv[" << nreq+i << "];\n";
        if (optional[i].o_svar) { fprintf(out, "\t\t%s = 1;\n", optional[i].o_svar); }
        fprintf(out, "\t} else {\n");
        *fout << "\t\t" << optional[i].sopt()
              <<  " = "
              << (optional[i].o_init == 0 ? "scheme_null" : optional[i].o_init)
              << ";\n";
        if (optional[i].o_svar) { *fout << "\t\t" << optional[i].o_svar << " = 0;\n"; }
        *fout << "\t}\n";
      }


      //
      //  If there are keys to process, or other parameters to init...
      //
      put_line_def();
      if (key_flag)
        {
          for (i = 0; i < nkey; i++) { *fout << "\tKEYS[" << i << "] = K" << keyword[i].ckey() << ";\n"; }

#if 0
          if(rest_flag)
            {
              fprintf(out, "\tparse_key(\"%s\", argc-%d, &argv[%d], %d, KEYS, KEY_VARS, &%s, %d);\n",
                      function, nreq+nopt, nreq+nopt, nkey, rest_var, allow_other_keys_flag);
            }
          else
            {
              fprintf(out, "\tparse_key(\"%s\", argc-%d, &argv[%d], %d, KEYS, KEY_VARS, 0, %d);\n",
                      function, nreq+nopt, nreq+nopt, nkey, allow_other_keys_flag);
            }
#else
          //  pass args starting at potential optional args
          if(rest_flag)
            {
              fprintf(out, "\tparse_key(\"%s\", argc-%d, &argv[%d], %d, KEYS, KEY_VARS, &__%s__, %d);\n",
                      function, nreq, nreq, nkey, rest_var, allow_other_keys_flag);
            }
          else
            {
              fprintf(out, "\tparse_key(\"%s\", argc-%d, &argv[%d], %d, KEYS, KEY_VARS, 0, %d);\n",
                      function, nreq, nreq, nkey, allow_other_keys_flag);
            }
#endif

          //  this code writes out the inits for those keys with default values
          put_line_def();
          for (i = 0;  i < nkey;  i++)
            {
              if (keyword[i].k_init == NULL) continue;
              fprintf(out, "\tif (SAME_OBJ(KEY_VARS[%d], scheme_null)) { %s = %s; }\n",
                      nkey+i, keyword[i].k_var_c.c_str(), keyword[i].k_init);
              if (keyword[i].k_svar != NULL)
                {
                  fprintf(out, "\t%s = !SAME_OBJ(%s, scheme_null);\n",
                          keyword[i].k_svar_c.c_str(), keyword[i].skey().c_str());
                }  //  if there is an default init value for the keyword
            }  //  loop over keywords
        }  //  if there are keyword arguments
    }  //  unknown condition

  for (i = 0;  i < naux;  i++)
    fprintf(out, "\t%s = %s;\n", aux[i].a_var,
	    aux[i].a_init == NULL ? Cnil : aux[i].a_init);
}



void put_ftail()
{
  int i;
  *fout << '\n';

  if(is_proc)
    {
#if 0
      *fout << "\tpe.stop();\n"
            << "\tpe.report();\n";
#endif
    }

  for (i = 0;  i < nkey;  i++) {
    *fout << "#undef " << keyword[i].skey() << "\n";
    if (keyword[i].k_svar != NULL) { *fout << "#undef " << keyword[i].k_svar << "\n"; }
  }
  *fout << "#undef check_type\n";
  *fout << "#undef defun_assert\n";
  *fout << "#undef defun_assert_cpp\n";
  *fout << "#undef defun_assert_args\n";
  *fout << "}\n";
}



void put_return()
{
#if 0
  int i, t;

  t = tab_save+1;
  if (nres == 0)
    fprintf(out, "VALUES(0) = Cnil;\n");
  else {
    fprintf(out, "{\n");
    for (i = 0;  i < nres;  i++) {
      put_tabs(t);
      fprintf(out, "VALUES(%d) = %s;\n", i, result[i]);
    }
    put_tabs(t);
    fprintf(out, "RETURN(%d);\n", nres);
    put_tabs(tab_save);
    fprintf(out, "}\n");
  }
  put_tabs(tab_save);
#endif
}



void main_loop()
{
  int c;
  char *p;

  lineno = 1;

 LOOP:
  reset();
#ifdef unix
  put_line_def();
  ///fprintf(out, "\n#line %d \"%s\"\n", lineno, filename);
#endif
  /*  while ((c = readc()) != '@') putc(c, out);*/

 keep_reading:
  c = readc();
  while(c != '\\' && c != '@') { putc(c, out); c = readc(); }
  if(c== '\\')
    {
      /* read one more char and see what it is */
      c = readc();
      if(c == '@') { putc(c, out); }
      else { putc('\\', out); putc(c, out); }
      goto keep_reading;
    }
  if ((c = readc()) != '(') error("@( expected");
  p = read_token();
  if (strcmp(p, "defun") == 0 || strcmp(p, "proc") == 0)
    {
      if(strcmp(p, "proc") == 0) { is_proc = 1; }
      else { is_proc = 0; }

      get_function();
      get_lambda_list();

      //
      //  add the function to the function sig list, sig list printed at end of source code
      //
      siglist.push_back(funcsig(nreq, string(function), string(cfunction)));

      put_init();
      put_fhead();
#ifdef unix
      fprintf(out, "\n#line %d \"%s\"\n", lineno, filename);
#endif
      while ((c = readc()) != '@') putc(c, out);
      put_declaration();

    BODY:
#ifdef unix
      fprintf(out, "\n#line %d \"%s\"\n", lineno, filename);
#endif
      while ((c = readc()) != '@') putc(c, out);
      //  just ate a '@'
      if ((c = readc()) == ')')
        {
          put_ftail();
        
          //  look for an optional ';', this helps indentation in emacs
          eat_optional_semicolon();

          goto LOOP;
        }
      else if (c != '(')
        { error("@( expected"); }
      p = read_token();
      if (strcmp(p, "return") == 0)
        {
          tab_save = tab;
          get_return();
          put_return();
          goto BODY;
        }
      else
        { error("illegal symbol"); }
    }
  else if(strncasecmp(p, "make-keyword", 12) == 0)
    {
      //  read @(make-keyword some-name)
      //  parser can now read the keyword name itself
      string keyname = read_token();

      //  read until next ')'
      while((c = readc()) != ')') putc(c, out);
      if(c == ')') 
        {
          //  output a def in it's place
          *fout << "Scheme_Object *K" << make_cfunction_name(keyname.c_str()) << "; ";
          putc(' ', out);
        }

      eat_optional_semicolon();

      //  add keyword to the keyword list
      klist.push_back(keywordsym(keyname));

      goto keep_reading;
    }
  else if(strncasecmp(p, "defvar", 6) == 0)
    {
      //  parsing the @(defvar varname value) construct
      string name = read_token();
      //string value = read_token();
      
      while((c = readc()) != ')') putc(c, out);
      if(c == ')') 
        {
          //  output a def in it's place
          *fout << "Scheme_Object *S" << make_cfunction_name(name.c_str()) << "; ";
          putc(' ', out);
        }
      eat_optional_semicolon();

      //  add keyword to the keyword list
      defvarlist.push_back(defvar(name));

      goto keep_reading;
    }
  else if(strncasecmp(p, "defconstant", 6) == 0)
    {
      //  parsing the @(defconstant varname value) construct
      string name = read_token();
      string value = read_token();

      while((c = readc()) != ')') putc(c, out);
      if(c == ')') 
        {
          //  output a def in it's place
          //*fout << "Scheme_Object *S" << make_cfunction_name(name.c_str()) << "; ";
          //putc(' ', out);
        }
      eat_optional_semicolon();

      //  add keyword to the keyword list
      defconstantlist.push_back(defconstant(name, value));

      goto keep_reading;
    }
  else if(strncasecmp(p, "MPP_HERE", 8) == 0)
    {
      //  put the tail here
      if((c=readc()) != ')') { error(") expected"); }
      if((c=readc()) != ';') { error("; expected"); }
      if(!already_put_tail) { put_siglist(); already_put_tail = 1; }
      goto LOOP;
    }
  else
    { error("illegal symbol"); }

}



int main(int argc, char **argv)
{
  char *p, *q;
  string outfile;
  bool outfile_p = 0;
  
  if (argc != 2) error("arg count");

  int op;
  while((op = getopt(argc, argv, "o:hp:t:")) != -1)
    {
      switch(op)
        {
        case 'p': prefix = optarg; break;
        case 't': postfix = optarg; break;
        case 'o': outfile = optarg; outfile_p = 1; break;
        case 'h': cout << "usage: mpp [options] filestem\n"
                    "-o outfile\n"
                    "-p prefix for vars\n"
                    "-t postfix for vars\n"; 
                  exit(0);
                  break;
        default:
          cerr << "unknown option "<< endl; exit(-1); break;
        }
    }

  strcpy(filename, argv[1]);
  filestem = make_cfunction_name(filename);
  strcat(filename, ".d");
  p = strrchr(argv[1], '/');
  if (p == NULL) p = argv[1];
  else p++;

  in = fopen(filename, "r");
  if (in == NULL) error("cannot open input file");
  
  if(!outfile_p) { 
    outfile = p;
    outfile += ".cc";
  }
  out = fopen(outfile.c_str(), "w");
  if (out == NULL) error("cannot open output file");

  fout = new ostdiostream(out);
  //printf("mpp: %s -> %s\n", filename, outfile);

  main_loop();
}
