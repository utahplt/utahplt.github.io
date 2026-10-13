/*
 *  Copyright (C) 1999 Gregory Lampshire.
 *  This software is provided 'as-is', without any express or implied
 *  warranty.  In no event will the authors be held liable for any damages
 *  arising from the use of this software.
 *
 *  Permission is granted to anyone to use this software for any purpose,
 *  including commercial applications, and to alter it and redistribute it
 *  freely, subject to the following restrictions:
 *
 *  1. The origin of this software must not be misrepresented; you must not
 *     claim that you wrote the original software. If you use this software
 *     in a product, an acknowledgment in the product documentation would be
 *     appreciated but is not required.
 *  2. Altered source versions must be plainly marked as such, and must not be
 *     misrepresented as being the original software.
 *  3. This notice may not be removed or altered from any source distribution.
 *
 *  Gregory Lampshire
 */
//
//  $Id: drscheme-utilities.cc,v 1.2 2000/04/10 01:46:59 gbol Exp gbol $
//

#include "drscheme-utilities.h"


Scheme_Object *Kallow_other_keys;

  //
  //  args should start on the value 1 after the required args, that is, subtract out
  //  the required args and adjust narg from the callers view.
  //
void parse_key(
               string fname,              //  the scheme version of the function name
               int narg,                  /* number of actual args */
               Scheme_Object **args,      /* actual args */
               int nkey,                  /* number of keywords */
               Scheme_Object **keys,      /* keywords for the function */
               Scheme_Object **vars,      /* where to put values (vars[0..nkey-1])
                                             and suppliedp (vars[nkey..2*nkey-1]) */
               Scheme_Object **rest,      /* rest variable or NULL */
               bool allow_other_keys)     /* whether other key are allowed */
{
  Scheme_Object **p;
  int i;
  Scheme_Object *k;

  /* fill in the rest arg list */
  if (rest != 0) {
    //  build a list out of the args
    *rest = scheme_build_list(narg, args);
  }

  //  Due to the way mzscheme fills in the argv struct that is passed to the
  //  does not breakout the required and optional arguments.
  //  Assume that args points to the start of potential optional arguments.
  //  Scan for valid keywords, then assume that this is the start of keyword
  //  arugments.  Adjust the args parameter. If no keywords found then
  //  do not adust args.
  for(i=0; i<narg ;i++)
    {
      //  for each argument, check whether it is a keyword parameter
      if(SCHEME_SYMBOLP(args[i]))
        {
          int j;
          for(j=0; j<nkey; j++)
            {
#if 0
              scheme_debug_print(keys[j]);
              cout << "\t" << flush;
              scheme_debug_print(args[i]);
              cout << endl;
#endif
              if(SAME_OBJ(keys[j], args[i]))
                {
                  args = &args[i];
                  narg = narg - i;
                  goto start_processing;
                }
            }
        }
    }
  //  if we arrive here, then no keywords were found!

 start_processing:

  //  default values: null, supplied: null
  for (i = 0; i < 2*nkey; i++) { vars[i] = scheme_null; }
  if (narg <= 0) return;

  /* scan backwards, so that if a keyword is duplicated, first one is used */
  args = args + narg;
 top:
  while (narg >= 2)
    {
      args = args - 2;
      k = args[0];
      //  search for key in keys list of allowable keys
      for (i = 0; i<nkey; i++)
        {
#if 0
          cout << "CHECKING: " << flush;
          scheme_debug_print(keys[i]);
          cout << '\t' << flush;
          scheme_debug_print(k);
          cout << endl;
#endif
          if(SAME_OBJ(keys[i], k)) {
            vars[i] = args[1];
            vars[nkey+i] = scheme_true;
            narg = narg-2;
            goto top;
          }
        }
      /* the key is a new one */
      if (allow_other_keys) { narg = narg-2; }
      else 
        {
          /* look for :allow-other-keys #t */
          for (i = narg-2, p = args; i >= 0; i -= 2, p -=2)
            if (SAME_OBJ(*p, Kallow_other_keys)) {
              allow_other_keys = !SCHEME_FALSEP(p[1]); break;
            }
          if (allow_other_keys) narg = narg-2;
          else 
            {
              scheme_signal_error("unrecognized key %s in call to function %s",
                                  SCHEME_SYM_VAL(k), fname.c_str());
            }
        }
    }
  if (narg != 0)
    scheme_signal_error("odd number of keys in call to function %s",
                        fname.c_str());
}




Scheme_Object *make_ordinary(Scheme_Env *env, Scheme_Object **obj, const char *name)
{
  scheme_register_extension_global((void*)obj, sizeof(Scheme_Object*));
  Scheme_Object *r = scheme_intern_symbol(name);
  return r;
}



Scheme_Object *make_keyword(Scheme_Env *env, Scheme_Object **obj, const char *name)
{
  scheme_register_extension_global((void*)obj, sizeof(*obj));
  *obj = scheme_intern_symbol(name);
  //  set it's value to be itself so it self-evaluates

  //  this code is from examining the output of mzc with (define :a ':a)
  Scheme_Bucket *bucket;
  bucket = scheme_global_bucket(*obj, env);
  scheme_set_global_bucket("define-values", bucket, *obj, 1);

  return *obj;
}



int sportbuf::overflow(int c = EOF)
{
  assert(port_);
  if(c != EOF)
    {
      const char cc = static_cast<char>(c);
      scheme_write_string(&cc, 1, port_);
    }
  return 1;
}

int sportbuf::sputc(int c)
{
  const char cc = static_cast<char>(c);
  xsputn(&cc, 1);
  return 1;
}

streamsize sportbuf::xsputn(const char *str, streamsize n)
{
  assert(port_);
  scheme_write_string((char*)str, long(n), port_);
  return n;
}

sportbuf::sportbuf(Scheme_Object *port)
{
  if(!SCHEME_OUTPORTP(port) && !SCHEME_INPORTP(port))
    {
      cerr << "port argument is not a scheme port object:" << endl;
      scheme_debug_print(port);
      cerr << endl;
      port_ = 0;
      return;
    }
  port_ = port;
}

int sportbuf::sync()
{
  assert(port_);
  scheme_flush_output(port_);
  return 0;
}

streamsize sportbuf::xsgetn(char *str, streamsize n)
{
  assert(0);
  //cout << __PRETTY_FUNCTION__ << endl;
  assert(port_);
  long r = long(n);
  r = scheme_get_chars(port_, r, str, 0);
  return int(r);
}

int sportbuf::underflow()
{
  assert(0);
  //cout << __PRETTY_FUNCTION__ << endl;
  if(!port_) { return EOF; }
  int c = scheme_getc(port_);
  scheme_ungetc(c, port_);
  return c;
}

int sportbuf::sgetc()
{
  if(!port_) { return underflow(); }
  return scheme_getc(port_);
}



void init_drscheme_utilities(Scheme_Env *env)
{
  Kallow_other_keys = make_keyword(env, &Kallow_other_keys, ":allow-other-keys");
}
