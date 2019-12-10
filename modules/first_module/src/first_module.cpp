#include <gilesc_module.h>

int plugin_is_GPL_compatible;

#define FIRST_MODULE_VERSION "v0.0.1"

EMACS_FUNCTION(meaning_of_life)
{
    send_message(env, "Hello Sailor!");
    EmacsValue result = env->make_integer(env, 42);
    return result;
}

EMACS_FUNCTION(first_module_version)
{
    EmacsValue result = env->make_string(env, FIRST_MODULE_VERSION, strlen(FIRST_MODULE_VERSION));
    return result;
}

extern s32
emacs_module_init(struct EmacsRuntime *runtime)
{
    // NOTE: get the emacs runtime environment
    EmacsEnv *env = runtime->get_environment(runtime);

    // NOTE: convert our feature string into quoted emacs symbols for
    // the elisp functions
    provide(env, "first_module");

    // NOTE: make a function callable by elisp. When elisp
    // `(first_module-version)` is evaluated, call the first_module_version
    // function above.
    EmacsValue fn_first_module = env->make_function(env, 0, 0, first_module_version, "Returns first_module version", 0);
    EmacsValue fn_meaning_of_life = env->make_function(env, 0, 0, meaning_of_life, "Returns the meaning of life", 0);

    bind_function(env, "first-module-version", fn_first_module);
    bind_function(env, "meaning-of-life", fn_meaning_of_life);

    return 0;
}

MODULE_TEST_MAIN(
    printf("Hello World\n");
    printf("First Module:\n\tVersion:%s, \n\tstrlen(version):%zu\n", FIRST_MODULE_VERSION, strlen(FIRST_MODULE_VERSION))
)
