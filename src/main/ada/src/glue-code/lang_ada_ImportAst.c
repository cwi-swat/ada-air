/*
Copyright (c) 2022, TNO (ESI) and NWO-I Centrum Wiskunde & Informatica (CWI)
All rights reserved.

Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

1. Redistributions of source code must retain the above copyright
notice, this list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright
notice, this list of conditions and the following disclaimer in the
documentation and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY
WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY
OF SUCH DAMAGE.

Authors:
Jurgen J. Vinju - Centrum Wiskunde & Informatica
Damien De Campos - TNO ESI
Pierre van de Laar - TNO ESI
*/

#include <jni.h>        // JNI header provided by JDK
#include <stdio.h>      // C Standard IO Header
#include "lang_ada_ImportAst.h"

extern const char *Ada_Export_File_Wrapper (const char *ada, const char *out);

extern const char* Ada_Export_Project_Wrapper(const char* ada, const char* out);


JNIEXPORT void JNICALL Java_lang_ada_ImportAst__1importAdaAst
  (JNIEnv *env, jobject thisObj, jstring adaFileName, jstring outFileName) {
  const char *ada = (*env)->GetStringUTFChars (env, adaFileName, NULL);
  const char *out = (*env)->GetStringUTFChars (env, outFileName, NULL);
  const char* e = Ada_Export_File_Wrapper (ada, out);
  if (e != NULL)
    {
      jclass Exception = (*env)->FindClass(env, "lang/ada/AdaException");
      (*env)->ThrowNew(env, Exception, e);
    }
}



JNIEXPORT void JNICALL Java_lang_ada_ImportAst__1importAdaProject
  (JNIEnv *env, jobject thisObj, jstring adaFileName, jstring outFileName) {
  const char *ada = (*env)->GetStringUTFChars (env, adaFileName, NULL);
  const char *out = (*env)->GetStringUTFChars (env, outFileName, NULL);
  const char* e = Ada_Export_Project_Wrapper (ada, out);
  if (e != NULL)
    {
      jclass Exception = (*env)->FindClass(env, "lang/ada/AdaException");
      (*env)->ThrowNew(env, Exception, e);
    }
}
