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
