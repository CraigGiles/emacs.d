#include "gilesc_module.h"

int plugin_is_GPL_compatible;


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

EMACS_FUNCTION(meaning_of_life)
{
    send_message(env, "Hello Sailor!");
    EmacsValue result = env->make_integer(env, 42);
    return result;
}

EMACS_FUNCTION(cmodule_version)
{
    EmacsValue result = env->make_string(env, "0.1", 3);
    return result;
}

extern s32
emacs_module_init(struct EmacsRuntime *runtime)
{
    // NOTE: get the emacs runtime environment
    EmacsEnv *env = runtime->get_environment(runtime);

    // NOTE: convert our feature string into quoted emacs symbols for
    // the elisp functions
    provide(env, "cmodule");

    // NOTE: make a function callable by elisp. When elisp
    // `(cmodule-version)` is evaluated, call the cmodule_version
    // function above.
    EmacsValue fn_cmodule = env->make_function(env, 0, 0, cmodule_version, "Returns cmodule version", 0);
    EmacsValue fn_meaning_of_life = env->make_function(env, 0, 0, meaning_of_life, "Returns the meaning of life", 0);

    bind_function(env, "cmodule-version", fn_cmodule);
    bind_function(env, "meaning-of-life", fn_meaning_of_life);

    return 0;
}

MODULE_TESTING_FUNION(
    printf("Hello World\n");
)
