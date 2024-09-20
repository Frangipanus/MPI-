#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include<stdbool.h>


struct character_set_s {
    int length;
    unsigned char *members;
};
typedef struct character_set_s character_set;

struct language_s {
    character_set first;
    character_set last;
    character_set allowed[256];
    bool has_empty;
};

typedef struct language_s language;
enum kleene_regex_kind{
    KLEENE_REGEX_EMPTY = 0,
    KLEENE_REGEX_EPSILON,
    KLEENE_REGEX_WORD,
    KLEENE_REGEX_STAR,
    KLEENE_REGEX_UNION,
    KLEENE_REGEX_CONCAT,
};

struct kleene_regex_s {
    enum kleene_regex_kind kind;
};
typedef struct kleene_regex_s kleene_regex;

struct kleene_regex_word_s{
    kleene_regex base;
    char* word;
};
typedef struct kleene_regex_word_s kleene_regex_word;
struct kleene_regex_binary_s{
    kleene_regex base;
    kleene_regex *left;
    kleene_regex *right;
};
typedef struct kleene_regex_binary_s kleene_regex_binary;
typedef struct kleene_regex_binary_s kleene_regex_union;
typedef struct kleene_regex_binary_s kleene_regex_concat;

struct kleene_regex_unary_s{
    kleene_regex base;
    kleene_regex* sub;
};
typedef struct kleene_regex_unary_s kleene_regex_star;
