#include <gilesc_module.h>

int plugin_is_GPL_compatible;

#define MODULE_NAME "scala-package"
#define MODULE_VERSION "v0.0.1"

internal u32
string_last_index_of(char* str, char value)
{
    u32 result = 1;
    // TODO
    return result;
}

internal char*
string_substring(MemoryArena *arena, char* str, u32 start, u32 end)
{
    char* result = str;
    // TODO
    return result;
}

internal char*
string_replace_all(MemoryArena *arena, char* str, char* from, char* to)
{
    char* result = str;
    // TODO
    return result;
}

internal char*
string_replace_all(char* str, char from, char to)
{
    char* result = str;
    // TODO
    return result;
}

internal char*
string_prepend(MemoryArena *arena, char* str, char* value)
{
    char* result = str;
    // TODO
    return result;
}

internal char*
remove_file_from_string(MemoryArena *arena, char* value)
{
    u32 last_slash_index = string_last_index_of(value, '/');
    Assert(last_slash_index > 0);

    printf("Last Slash Index = %u\n", last_slash_index);
    char* result = ReserveMemoryForString(arena, last_slash_index);
    result = string_substring(arena, value, 0, last_slash_index);

    return result;
}

internal char*
convert_path_to_package_statement(MemoryArena *arena, char* path)
{
    // char buffer[1024] = {};
    char* result = ReserveMemoryForString(arena, strlen(path));

    path = remove_file_from_string(arena, path);
    path = eat_string_including(path, "src/main/scala/");
    // path = string_replace_all(arena, path, "/", "."); // TODO do i want this one or the char one?
    path = string_replace_all(path, '/', '.');
    path = string_prepend(arena, path, "package ");
    printf("Got result %s\n", path);

    return result;
}

EMACS_FUNCTION(scala_insert_inline_package_statement)
{
    // (save-excursion
    //   (beginning-of-buffer)
    //   (insert "package ")
    // )
    EmacsValue beginning_of_buffer_symbol = env->intern(env, "beginning-of-buffer");
    EmacsValue insert_package_symbol = env->intern(env, "insert");

    EmacsValue args_excursion[] = {
        beginning_of_buffer_symbol,
        insert_package_symbol
    };

    invoke_elisp_function(env, "save-excursion", 2, args_excursion);

    EmacsValue goto_args = env->make_integer(env, 1);
    EmacsValue insert_args = env->make_string(env, "package ", strlen("package "));

    invoke_elisp_function(env, "goto-char", 1, &goto_args);
    EmacsValue result = invoke_elisp_function(env, "insert", 1, &insert_args);

    // insert row

    // EmacsValue insert_args = env->make_string(env, "package ", strlen("package "));
    // invoke_elisp_function(env, "insert", 1, &insert_args);
    return result;
}

// EMACS_FUNCTION(scala_is_package_defined)
// {
//     EmacsValue expected = env->make_string(env, "package ", strlen("package "));
//     EmacsValue b_args[] = {
//         env->make_integer(env, 1),
//         env->make_integer(env, 10)
//     };

//     EmacsValue buffer_chunk = invoke_elisp_function(env, "buffer-substring", 2, b_args);
//     EmacsValue result = env->intern(env, "nil");
//     char got[1024];
//     long got_length;
//     env->copy_string_contents(env, buffer_chunk, got, &got_length);
//     send_message(env, got);
//     // emacs_value (*intern) (emacs_env *env,
//     //                        const char *symbol_name)

//     return result; //env->eq(env, buffer_chunk, expected);
// }

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
    // bind_function(env, "scala-is-package-defined", scala_is_package_defined,
    //               0, 0, "Returns t if the package is already defined, nil otherwise", 0);
    bind_function(env, "scala-insert-inline-package-statement", scala_insert_inline_package_statement,
                  0, 0, "Inserts the package statement at the top of the buffer", 0);

    return 0;
}

MODULE_TEST_MAIN(
    printf("%s -- Version: %s\n", MODULE_NAME, MODULE_VERSION);

    // NOTE: this is how you would create an initialize a memory arena
    MemoryArena arena = {};
    create_memory_arena(&arena);

    char *expected = "package com.mycomp.myproject.mypackage";
    char *path = "/Users/gilesc/Development/myproject/src/main/scala/com/mycomp/myproject/mypackage/myfile.scala";
    char *result = convert_path_to_package_statement(&arena, path);

    Assert(strcmp(result, expected) == 0)
    printf("%s\n", result);
)
