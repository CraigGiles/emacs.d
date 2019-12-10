MAIN_FILE=first_module.cpp
EXE_NAME=first_module
RELEASE_PATH=../releases/$EXE_NAME

COMPILE_FLAGS="-g \
               -O0 \
               -Wall \
               -Werror \
               -Wno-write-strings \
               -Wno-unused-variable \
               -Wno-unused-value \
               -Wno-unused-function \
               -Wno-missing-braces \
               -Wno-sign-compare \
	       -std=gnu++11 \
               -fno-rtti \
               -fno-exceptions \
               -Wno-shift-negative-value" # NOTE(craig): Need this for the stb libraries. 

RELEASE_FLAGS="-g \
               -O2 \
               -Wall \
               -Werror \
               -Wno-write-strings \
               -Wno-unused-variable \
               -Wno-unused-value \
               -Wno-unused-function \
               -Wno-missing-braces \
               -Wno-sign-compare \
	       -std=gnu++11 \
               -fno-rtti \
               -fno-exceptions \
               -Wno-shift-negative-value" # NOTE(craig): Need this for the stb libraries. 

DEBUG_FLAGS="-DGILESC_DEBUG=1"
LD_FLAGS=""
EXE_HEADER_INCLUDES="-Isrc -I../lib"

function clean_target_directory {
    echo "cleaning up old target folder"
    rm -rf target
    mkdir -p target
}

function build_module {
    echo "compiling module"
    g++ $COMPILE_FLAGS $EXE_HEADER_INCLUDES $DEBUG_FLAGS -shared ./src/$MAIN_FILE -o ./target/$EXE_NAME.so
    g++ $COMPILE_FLAGS $EXE_HEADER_INCLUDES $DEBUG_FLAGS ./src/$MAIN_FILE -o ./target/$EXE_NAME.out
}

function test_module {
    echo "testing module"
    echo "--------------"
    ./target/$EXE_NAME.out
}

function release_module {
    echo "compiling module"
    g++ $RELEASE_FLAGS $EXE_HEADER_INCLUDES  -shared ./src/$MAIN_FILE -o $RELEASE_PATH.so
    rm -rf $RELEASE_PATH.so.dSYM
}

if [ "$1" = "release" ]
then
    release_module
else
    echo ""
    echo "Starting Build"
    echo "--------------"
    clean_target_directory
    build_module
    test_module
fi
