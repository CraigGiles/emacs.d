// ==================================================
//    Current Tasks
// --------------------------------------------------
// TODO(craig): make this only work on scala files
// TODO(craig): Make this work with stacked packages
// TODO(craig): 

#include <gilesc_module.h>

#include <string.h>
int plugin_is_GPL_compatible;

#define MODULE_NAME "scala-package"
#define MODULE_VERSION "v1.0.0"

#define BUFFER_SIZE 1024

// TODO(craig): this may be better in gilesc_module.h
#define Nil env->intern(env, "nil"); 

internal b32
is_valid_scala_path(MemoryArena *arena, char* value)
{
    b32 result = false;

    u32 last_index = string_last_index_of(value, '.');
    Assert(last_index > 0);
    char* file = string_substring(arena, value, last_index + 1);
    result = strcmp(file, "scala") == 0;

    return result;
}

internal char*
remove_file_from_string(MemoryArena *arena, char* value)
{
    u32 last_slash_index = string_last_index_of(value, '/');
    Assert(last_slash_index > 0);

    char* result = string_substring(arena, value, 0, last_slash_index);

    return result;
}

internal char*
convert_path_to_package_statement(MemoryArena *arena, char* path, b32 stacked = false)
{
    char* result = ReserveMemoryForString(arena, strlen(path));

    path = remove_file_from_string(arena, path);
    path = eat_string_including(path, "src/main/scala/");
    path = string_replace_all(path, '/', '.');
    path = string_prepend(arena, path, "package ");

    if (stacked)
    {
        // TODO(craig) convert to `package com.org\npackage system\npackage something`
    }

    strcpy(result, path);

    return result;
}

EMACS_FUNCTION(scala_create_nested_package_statement)
{
    MemoryArena arena = {};
    STACK_MEMORY_ARENA(&arena);
            
    char buffer[BUFFER_SIZE] = {};
    long length = BUFFER_SIZE;

    EmacsValue fn_file_name = env->intern(env, "buffer-file-name"); 
    EmacsValue file_name_result = env->funcall(env, fn_file_name, 0, 0);
    env->copy_string_contents(env, file_name_result, buffer, &length);
    char* package_result = convert_path_to_package_statement(&arena, buffer);

    EmacsValue package_to_insert = env->make_string(env, package_result, strlen(package_result));
    EmacsValue fn_beg_of_buffer = env->intern(env, "beginning-of-buffer"); 
    EmacsValue fn_insert = env->intern(env, "insert"); 
    EmacsValue fn_save_excursion = env->intern(env, "save-excursion"); 

    EmacsValue excursion_functions[] = { fn_beg_of_buffer };

    EmacsValue result = env->funcall(env, fn_save_excursion, 1, excursion_functions);

    return Nil;
}

#define CREATE_PACKAGE_DOCUMENT \
    "Create an inline package statement text to be inserted to the\n" \
    "top of your current scala file.\n" \
    "\n" \
    "ARG1: The full path to the file being edited by the\n" \
    "      buffer. Usually obtained by the command 'buffer-file-name\n" \
    "ARG2: Optional parameter that dictates if the package should be\n" \
    "      stacked or inline. Pass in `t` for inline, `nil` for stacked\n"

EMACS_FUNCTION(scala_create_package_statement)
{
    MemoryArena arena = {};
    STACK_MEMORY_ARENA(&arena);

    EmacsValue result = Nil;
    b32 stacked_packages = false;

    if (nargs == 2 && env->is_not_nil(env, args[1]))
    {
        stacked_packages = true;
    }

    if (nargs >= 1)
    {
        char buffer[BUFFER_SIZE] = {};
        long length = BUFFER_SIZE;
        
        EmacsValue path_em = args[0];
        if (env->copy_string_contents(env, path_em, buffer, &length))
        {
            // NOTE successfully copied
            char* path = ReserveMemoryForString(&arena, length);
            strncpy(path, buffer, length);

            if (is_valid_scala_path(&arena, path))
            {
                char* package_result = convert_path_to_package_statement(&arena, path, stacked_packages);
                result = env->make_string(env, package_result, strlen(package_result));
            }
            else
            {
                result = Nil;
            }

        } else {
            // NOTE unsuccessful in copying 
            result = Nil;
        }
    }
    
    return result;
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
    bind_function(env, "scala-create-package-statement", scala_create_package_statement,
                  1, 2 ,CREATE_PACKAGE_DOCUMENT , 0);

    return 0;
}

int main() {
    printf("%s -- Version: %s\n\n", MODULE_NAME, MODULE_VERSION);

    // NOTE: this is how you would create an initialize a memory arena
    MemoryArena arena = {};
    STACK_MEMORY_ARENA(&arena);

    char *expected = "package com.mycomp.myproject.mypackage";
    char *path = "/Users/gilesc/Development/myproject/src/main/scala/com/mycomp/myproject/mypackage/myfile.scala";

    char *result = convert_path_to_package_statement(&arena, path);
    printf("Expected(%s)\n", expected);
    printf("ConvertedTo(%s)\n", result);

    Assert(strcmp(result, expected) == 0);

    Assert(is_valid_scala_path(&arena, path) == true);
    Assert(is_valid_scala_path(&arena, "*scratch*") == false);
}
