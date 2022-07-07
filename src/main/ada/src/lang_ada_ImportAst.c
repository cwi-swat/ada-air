#include <jni.h>        // JNI header provided by JDK
#include <stdio.h>      // C Standard IO Header
#include "lang_ada_ImportAst.h"

extern void Ada_Func_Wrapper(const char* ada, const char* out);


JNIEXPORT void JNICALL Java_lang_ada_ImportAst__1importAdaAst
  (JNIEnv *env, jobject thisObj, jstring adaFileName, jstring outFileName) {
  const char *ada = (*env)->GetStringUTFChars (env, adaFileName, NULL);
  const char *out = (*env)->GetStringUTFChars (env, outFileName, NULL);
  Ada_Func_Wrapper (ada,out);
}
