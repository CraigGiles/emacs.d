#if !defined(GILESC_STRING_H)
/* ========================================================================
   $File: $
   $Date: $
   $Revision: $
   $Creator: Craig Giles $
   $Notice: (C) Copyright 2019 by Craig Giles. All Rights Reserved. $
   ======================================================================== */

#define GILESC_STRING_H

#include <ctype.h>

internal b32
string_begins_with(char *str, char *value)
{
    size_t lenpre = strlen(value),
           lenstr = strlen(str);

    b32 result = lenstr < lenpre ? false : strncmp(value, str, lenpre) == 0;

    return result;
}

internal char*
eat_spaces(char* str)
{
    char* result = "";
    if (str) {
        while(isspace((unsigned char)*str)) str++;
        result = str;
    }
    return result;
}

internal char*
trim(char *str)
{
    char *end;

    // Trim leading space
    while(isspace((unsigned char)*str)) str++;

    if(*str == 0)  // All spaces?
        return str;

    // Trim trailing space
    b32 modified = false;
    end = str + strlen(str) - 1;
    while(end > str && isspace((unsigned char)*end)) {
        modified = true;
        end--;
    }

    // Write new null terminator character
    if (modified) {
        end[1] = '\0';
    }

    return str;
    #if 0
    char* result = eat_spaces(str);

    size_t length = strlen(result);
    char c = result[length-1];

    while (char c == ' ') {
        result--;
        c = result[length-1];
    }
    #endif
}

internal b32
string_is_empty(char *str)
{
    b32 result = false;
    if (strcmp(str, "") == 0)
    {
        result = true;
    } else {
        result = (strlen(trim(str)) == 0);
    }
    return result;
}

// from the beginning of the string, remove all contents up to and
// including the 'value' If the string is "Hello! World!" and you call
// eat_string_including(str, "!") then the result of the function will
// be " World!"
internal char*
eat_string_including(char* str, char* value)
{
    while (strcmp(value, "") != 0) {
        if (string_begins_with(str, value)) value++;
        str++;
    }

    return str;
}

internal char*
replace_all(char* str, char* old, char* value)
{
    char* at = str;
    // while (strcmp(at, "") != 0) {
    //     if (!string_begins_with(at, old)) at++;
    // }

    return str;
}


#endif
