Assumptions; my c code style is very different than a lot of other
engineers that i've met in the field and so I've created some macros
that will allow me to continue to use that style without changing the
emacs-module.h. This style is something akin to:

```c
MyType my_variable = ...

static return_type
function_name_here(...)
{
    // ...
}
```

To support this I've created a few macros which became the start of my
`gilesc_module.h` library. They're simply:

```c
#define EmacsEnv emacs_env
#define EmacsValue emacs_value
#define EmacsRuntime emacs_runtime
```

As I go deeper into modules, no doubt I will need to add more there. But lets move on.



Writing Dynamic Modules (from GNU):
https://www.gnu.org/software/emacs/manual/html_node/elisp/Writing-Dynamic-Modules.html

The first crack at this;
i have my own little helpers with so far, nothing but macros that conver the things to sensible (for me) types. Since my style for coding is 



Step 1: Understanding how to build out a hello world

```c
    // NOTE: get the emacs runtime environment
    EmacsEnv *env = runtime->get_environment(runtime);

    // -------------------------------------------------------------------------
    // NOTE: provide function:
    // provide is a built-in function in ‘C source code’.
    //
    // (provide FEATURE &optional SUBFEATURES)
    //
    // Announce that FEATURE is a feature of the current Emacs.
    // The optional argument SUBFEATURES should be a list of symbols listing
    // particular subfeatures supported in this version of FEATURE.
    // -------------------------------------------------------------------------
    // NOTE: convert our feature string into quoted emacs symbols for the elisp functions
    EmacsValue feature_symbol = env->intern(env, "cmodule");
    EmacsValue provide_args[] = { feature_symbol };
    EmacsValue provide_symbol = env->intern(env, "provide");
    s32 provide_number_of_args = 1;
    // NOTE: emacs env, value, number of args we're taking, and the actual args
    env->funcall(env, provide_symbol, provide_number_of_args, provide_args);

    // -------------------------------------------------------------------------
    // NOTE: Elisp fset function:
    // fset is a built-in function in ‘C source code’.
    //
    // (fset SYMBOL DEFINITION)
    //
    // S
    // -------------------------------------------------------------------------
    EmacsValue fset_symbol = env->intern(env, "fset");

    // NOTE: Bind the ELisp function (cmodule-version) to call Fcmodule_version
    EmacsValue cmodule_version_function = env->make_function(env, 0, 0, cmodule_version,
                                                             "Returns cmodule version", NULL);
    EmacsValue cmodule_version_symbol = env->intern(env, "cmodule-version");
    EmacsValue cmodule_args[] = { cmodule_version_symbol, cmodule_version_function };
    s32 cmodule_number_of_args = 2;
    env->funcall(env, fset_symbol, cmodule_number_of_args, cmodule_args);
```

First extranction for bind_function
```c
internal void
bind_function(EmacsEnv *env, const char *function_name, EmacsFunctionPointer function_pointer,
              s32 argument_count, const char* document, void* data)
{
    // -------------------------------------------------------------------------
    // NOTE: Elisp fset function:
    // fset is a built-in function in ‘C source code’.
    //
    // (fset SYMBOL DEFINITION)
    //
    // S
    // -------------------------------------------------------------------------
    EmacsValue fset_symbol = env->intern(env, "fset");

    // NOTE: Bind the ELisp function (cmodule-version) to call Fcmodule_version
    EmacsValue cmodule_version_function = env->make_function(env, 0, argument_count, function_pointer, document, data);
    EmacsValue cmodule_version_symbol = env->intern(env, function_name);
    EmacsValue fset_args[] = { cmodule_version_symbol, cmodule_version_function };
    s32 fset_number_of_args = 2;

    env->funcall(env, fset_symbol, fset_number_of_args, fset_args);
}

...

bind_function(env, "cmodule-version", cmodule_version, 0, document, 0);
```

Final version of the entire plugin
```c
EMACS_FUNCTION(cmodule_version)
{
    EmacsValue result = env->make_string(env, "0.1", 3);
    return result;
}

EMACS_FUNCTION(mymod_test)
{
    EmacsValue result = env->make_integer(env, 42);
    return result;
}

extern
EMACS_MAIN(runtime)
{
    // NOTE: get the emacs runtime environment
    EmacsEnv *env = runtime->get_environment(runtime);

    // NOTE: convert our feature string into quoted emacs symbols for the elisp functions
    provide(env, "cmodule");

    // NOTE: make a function callable by elisp. When elisp
    //   `(cmodule-version)` is evaluated, call the
    //   cmodule_version function above.
    EmacsValue fn = env->make_function(env, 0, 0, cmodule_version, "Returns cmodule version", 0);
    bind_function(env, "cmodule-version", fn);

    return 0;
}
```

OTHER NOTES:
https://archive.fosdem.org/2019/schedule/event/extend_emacs_2019/attachments/slides/3357/export/events/attachments/extend_emacs_2019/slides/3357/FOSDEM19_emacs_modules.pdf

Any emacs_value created inside a module function will be freed after it returns
```
emacs_value foo (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    emacs_value a = env->make_integer(env, 42);
    emacs_value b = env->make_integer(env, 43);
    return env->make_integer(env, 44);
    /* a and b automatically freed */
}
```

Creating a globally reference counted value
```
emacs_value Qnil, Qt; // Global Variable
int emacs_module_init (emacs_runtime *rt)
{
    emacs_env *env = rt->get_environment(rt);
    Qt = env->make_global_ref(env, env->intern(env, "t"));
    Qnil = env->make_global_ref(env, env->intern(env, "nil"));
    /* ...register foo()... */
    return 0;
}

emacs_value foo (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    /* OK to use Qnil and Qt here */
}
```
