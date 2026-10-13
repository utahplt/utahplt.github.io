/* -*- C++ -*- */
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
//  $Id: drscheme-utilities.h,v 1.2 2000/04/10 01:46:44 gbol Exp gbol $
//

#if !defined(DRSCHEME_UTILITIES_H_)
#define DRSCHEME_UTILITIES_H_

#include <string>
#include "scheme.h"
#include <streambuf.h>


extern Scheme_Object *Kallow_other_keys;

extern Scheme_Object *make_keyword(Scheme_Env *env, Scheme_Object **obj, const char *name);
extern Scheme_Object *make_ordinary(Scheme_Env *env, Scheme_Object **obj, const char *name);

extern void parse_key(
                      string fname,              //  the scheme version of the function name
                      int narg,                  /* number of actual args */
                      Scheme_Object **args,      /* actual args */
                      int nkey,                  /* number of keywords */
                      Scheme_Object **keys,      /* keywords for the function */
                      Scheme_Object **vars,      /* where to put values (vars[0..nkey-1])
                                                    and suppliedp (vars[nkey..2*nkey-1]) */
                      Scheme_Object **rest,      /* rest variable or NULL */
                      bool allow_other_keys);    /* whether other key are allowed */


extern void init_drscheme_utilities(Scheme_Env *env);
//
//  This needs to be called before using the parse_key() function.
//  It should go in your startup code.
//




//
//  Quick hack to make iostreams work with mzscheme port objects.
//  It has only been tested with output streams and will *not*
//  work for input streams.
//
class sportbuf : public streambuf
{
protected:
  Scheme_Object *port_;
public:
  sportbuf(Scheme_Object *port);

  //  get area
  int sgetc();  //  calls underflow() if not data available

  //  put area
  streamsize sputn(char *s, streamsize n) { return xsputn(s, n); }
  int sputc(int c);


  int sync();

protected:

  streamsize xsgetn(char *str, streamsize n);
  int underflow();

  
  int overflow(int c = EOF);
  streamsize xsputn(const char *str, streamsize n);

};


#endif //  DRSCHEME_UTILITIES_H_
