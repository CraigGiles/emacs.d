#include <gilesc_module.h>

int plugin_is_GPL_compatible;

#define MODULE_NAME "scala-package"
#define MODULE_VERSION "v0.0.1"

EMACS_FUNCTION(scala_insert_inline_package_statement)
{
    EmacsValue goto_args = env->make_integer(env, 1);
    EmacsValue insert_args = env->make_string(env, "package ", strlen("package "));

    invoke_elisp_function(env, "goto-char", 1, &goto_args);
    EmacsValue result = invoke_elisp_function(env, "insert", 1, &insert_args);

    // insert row

    // EmacsValue insert_args = env->make_string(env, "package ", strlen("package "));
    // invoke_elisp_function(env, "insert", 1, &insert_args);
    return result;
}

EMACS_FUNCTION(scala_is_package_defined)
{
    EmacsValue expected = env->make_string(env, "package ", strlen("package "));
    EmacsValue b_args[] = {
        env->make_integer(env, 1),
        env->make_integer(env, 10)
    };

    EmacsValue buffer_chunk = invoke_elisp_function(env, "buffer-substring", 2, b_args);
    EmacsValue result = env->intern(env, "nil");
    char got[1024];
    long got_length;
    env->copy_string_contents(env, buffer_chunk, got, &got_length);
    send_message(env, got);
    // emacs_value (*intern) (emacs_env *env,
    //                        const char *symbol_name)

    return result; //env->eq(env, buffer_chunk, expected);
}

EMACS_FUNCTION(scala_package_version)
{
    EmacsValue result = env->make_string(env, MODULE_VERSION, strlen(MODULE_VERSION));
    return result;
}

extern s32
emacs_module_init(struct EmacsRuntime *runtime)
{
    EmacsEnv *env = runtime->get_environment(runtime);
    provide(env, "scala-package");

    bind_function(env, "scala-package-version", scala_package_version,
                  0, 0, "Returns the version for the scala-package module", 0);
    bind_function(env, "scala-is-package-defined", scala_is_package_defined,
                  0, 0, "Returns t if the package is already defined, nil otherwise", 0);
    bind_function(env, "scala-insert-inline-package-statement", scala_insert_inline_package_statement,
                  0, 0, "Inserts the package statement at the top of the buffer", 0);

    return 0;
}

MODULE_TEST_MAIN(
    // NOTE: this is how you would create an initialize a memory arena
    MemoryArena arena = {};
    create_memory_arena(&arena);

    printf("%s -- Version: %s\n", MODULE_NAME, MODULE_VERSION);
)
