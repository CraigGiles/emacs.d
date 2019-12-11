#if !defined(GILESC_MODULE_H)
/* ========================================================================
   $File: $
   $Date: $
   $Revision: $
   $Creator: Craig Giles $
   $Notice: (C) Copyright 2019 by Craig Giles. All Rights Reserved. $
   ======================================================================== */

#define GILESC_MODULE_H

#include <emacs-module.h>

#include "gilesc_types.h"

#include <stdio.h> 
#include <string.h> 
#include <ctype.h>

#if GILESC_DEBUG
#define MODULE_TEST_MAIN(stuff) int main() { stuff; return 0; }
#else
#define MODULE_TEST_MAIN(stuff)
#endif

#define EmacsEnv emacs_env
#define EmacsValue emacs_value
#define EmacsRuntime emacs_runtime

//
// Create a function named `name` that can be bound and callable by elisp.
// Example usage:
//     EMACS_FUNCTION(my_function)
//     {
//         EmacsValue result = env->make_integer(env, 42);
//         return result;
//     }
//
#define EMACS_FUNCTION(name) \
    internal EmacsValue name(EmacsEnv *env, ptrdiff_t nargs, EmacsValue args[], void *data)

typedef EmacsValue (*EmacsFunctionPointer)(EmacsEnv *env, ptrdiff_t nargs, EmacsValue args[], void *data);

//
// call an elisp function named `name` with the specified function arguments
//
internal EmacsValue
invoke_elisp_function(EmacsEnv *env, const char* name, u32 arg_count, EmacsValue *args)
{
    // TODO(craig): handle any errors with emacs either before or after these function calls
    EmacsValue fn = env->intern(env, name);
    EmacsValue result = env->funcall(env, fn, arg_count, args);
    return result;
}

//
// Displays a message at the bottom of the users screen
//
internal void
send_message(EmacsEnv *env, const char* msg)
{
    size_t len = strlen(msg);

    if (len)
    {
        EmacsValue args = env->make_string(env, msg, len-1 /* cut trailing newline */);
        invoke_elisp_function(env, "message", 1, &args);
    }
}

//
// Provide a feature to emacs that can be `(require 'feature)` by the
// end-user. Documentation from gnu.org for `Named-Features`
//
//   A feature name is a symbol that stands for a collection of
//   functions, variables, etc. The file that defines them should
//   provide the feature. Another program that uses them may ensure
//   they are defined by requiring the feature. This loads the file of
//   definitions if it hasn't been loaded already.
//
// https://www.gnu.org/software/emacs/manual/html_node/elisp/Named-Features.html
//
internal void
provide(EmacsEnv *env, const char* feature)
{
    s32 provide_number_of_args = 1;
    EmacsValue feature_symbol = env->intern(env, feature);
    EmacsValue provide_args[] = { feature_symbol };

    // -------------------------------------------------------------------------
    // NOTE: elisp provide function:
    // provide is a built-in function in ‘C source code’.
    //
    // (provide FEATURE &optional SUBFEATURES)
    //
    // Announce that FEATURE is a feature of the current Emacs.
    // The optional argument SUBFEATURES should be a list of symbols listing
    // particular subfeatures supported in this version of FEATURE.
    // -------------------------------------------------------------------------
    invoke_elisp_function(env, "provide", provide_number_of_args, provide_args);
}

//
// Binds a function we've created to be callable by ELisp using the
// `function_name` that is provided by the caller.
//
internal void
bind_function(EmacsEnv *env, const char *function_name, EmacsValue fn)
{
    EmacsValue function_symbol = env->intern(env, function_name);
    EmacsValue fset_args[] = { function_symbol, fn };
    s32 fset_number_of_args = 2;

    // -------------------------------------------------------------------------
    // NOTE: Elisp fset function:
    // fset is a built-in function in ‘C source code’.
    //
    // (fset SYMBOL DEFINITION)
    //
    // S
    // -------------------------------------------------------------------------
    invoke_elisp_function(env, "fset", fset_number_of_args, fset_args);
}

internal void
bind_function(EmacsEnv *env, const char *function_name, EmacsFunctionPointer function_pointer,
              s32 min_argument_count, s32 max_argument_count, const char* document, void* data)
{
    // NOTE: The max_arity argument can have the special value emacs_variadic_function
    EmacsValue fn = env->make_function(env, min_argument_count, max_argument_count,
                                       function_pointer, document, data);
    bind_function(env, function_name, fn);
}

//
// IMPORTANT: -----------------------------------------------------
//
// TODO(craig) -- below are stuff that i'm just playing around with
//   and not totally in love with keeping
//
#define NonLocalExitEnum emacs_funcall_exit

internal b32
is_function_call_error_set(EmacsEnv *env)
{
    b32 result = false;
    NonLocalExitEnum en = env->non_local_exit_check(env);
    
    switch (en)
    {
        /* Function has returned normally.  */
        case emacs_funcall_exit_return: { result = false; } break;

        /* Function has signaled an error using `signal'.  */
        case emacs_funcall_exit_signal: { result = true; } break;

        /* Function has exit using `throw'.  */
        case emacs_funcall_exit_throw: { result = true; } break;

        /* Error out if we get any other case */
        InvalidDefaultCase;
    };

    return result;
}

internal void
make_global_reference(EmacsEnv *env, EmacsValue value)
{
    env->make_global_ref(env, value);
    if (is_function_call_error_set(env)) {
        env->free_global_ref(env, value); // TODO(craig): is this the right thing to do?
    }
}

#endif
