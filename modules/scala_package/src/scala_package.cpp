#include <gilesc_module.h>

int plugin_is_GPL_compatible;

#define MODULE_NAME "scala-package"
#define MODULE_VERSION "v0.0.1"

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

    return 0;
}

MODULE_TEST_MAIN(
    printf("%s:\n\tVersion:%s, \n\tstrlen(version):%zu\n",
           MODULE_NAME, MODULE_VERSION, strlen(MODULE_VERSION))
)
