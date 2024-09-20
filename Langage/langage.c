#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include<stdbool.h>
#include"langage.h"

bool ll_belongs(const character_set set, unsigned const char letter){
    int deb = 0;
    int fin = set.length-1;
    while ( fin!=deb){
        if(set.members[(fin+deb)/2] == letter){
            return true;
        }
        else{
            if ( (set.members[fin+deb])/2 > letter){
                deb = (fin + deb)/2;
            }
            else{
                fin = (fin + deb)/2 + 1;
            }
        }
    }
    return false;
}

bool ll_match(const language language, const char* word){
    int length = strlen(word);
    if(length == 0){
        return language.has_empty;
    }
    if(!(ll_belongs(language.first, word[0]))){
        return false;
    }
    for(int i = 1; i<length-1; i++){
        int ind = word[i];
        if(!(ll_belongs(language.allowed[ind], word[i]))){
            return false;
        }
    }
    if(!ll_belongs(language.last, word[length-1])){
        return false;
    }
    return true;
}

int __max(int a, int b){
    if(a>b){
        return a;
    }
    return b;
}

character_set reunion(character_set lang1, character_set lang2){
    int length = 0;
    int ind1=0;
    int ind2=0;
    while(ind1 < lang1.length && ind2 < lang2.length){
        if( lang1.members[ind1] == lang2.members[ind2]){
            length = length + 1;
            ind1 = ind1 + 1;
            ind2 = ind2 + 1;
        }
        else if( lang1.members[ind1] > lang2.members[ind2]){
            ind2 = ind2 + 1;
            length = length + 1;
        }
        else{
            ind1 = ind1 + 1;
            length = length + 1;
        }
    }
    length  = length + (ind1 - lang1.length) + (ind2 - lang2.length);
    char* firsty = (char*)malloc(length*sizeof(char));
    int ind11=0;
    int ind22=0;
    int ind = 0;
    while(ind11 < lang1.length && ind22 < lang2.length){
        if( lang1.members[ind11] == lang2.members[ind]){
            firsty[ind]= lang1.members[ind11];
            ind = ind + 1;
            ind11 = ind11 + 1;
            ind22 = ind22 + 1;
        }
        else if( lang1.members[ind11] > lang2.members[ind22]){
            firsty[ind]= lang1.members[ind22];
            ind22 = ind22 + 1;
            ind = ind + 1;
            
        }
        else{
            firsty[ind]= lang1.members[ind11];
            ind11 = ind11 + 1;
            ind = ind + 1;
        }
    }
    character_set first;
    first.length = length;
    first.members = firsty;
    return first;
}

language ll_reunion(const language lang1, const language lang2){
    language res;
    res.first = reunion(lang1.first, lang2.first);
    res.last = reunion(lang1.last, lang2.last);
    
    for(int i = 0; i < 256; i++){
        res.allowed[i] = reunion(lang1.allowed[i], lang2.allowed[i]);
    }
    res.has_empty = lang1.has_empty || lang2.has_empty;
    return res;
}

language ll_concat(const language lang1, const language lang2){
    language res;
    if(lang1.has_empty){
        res.first = reunion(lang1.first, lang2.first);
    }
    else{
        res.first = lang1.first;
    }
    if(lang2.has_empty){
        res.last = reunion(lang2.last, lang1.last);
    }
    else{
        res.last = lang2.last;
    }

    
    res.has_empty = lang1.has_empty && lang2.has_empty;
    for(int i = 0; i < 256; i++){
        res.allowed[i] = reunion(lang1.allowed[i], lang2.allowed[i]);
    }
    for(int i = 0; i < lang1.last.length; i++){
        res.allowed[lang1.last.members[i]] = reunion(lang1.allowed[lang1.last.members[i]], lang2.first); 
    }
    return res;
}

language ll_star(const language lang){
    language res;
    res.has_empty = true;
    res.first = lang.first;
    res.last = lang.last;
    for(int i = 0; i < lang.last.length; i++){
        res.allowed[lang.last.members[i]] = reunion(lang.allowed[lang.last.members[i]], lang.first); 
    }
    return res;
}

kleene_regex* kleene_empty(){
    kleene_regex* res = (kleene_regex*)malloc(sizeof(kleene_regex));
    res->kind = KLEENE_REGEX_EMPTY;
    return res;
}

kleene_regex* kleen_epsilon(){
    kleene_regex* res = malloc(sizeof(kleene_regex));
    res->kind = KLEENE_REGEX_EPSILON;
    return res;
}

kleene_regex* kleene_word(char*  word){
    kleene_regex_word* acc = malloc(sizeof(kleene_regex_word));
    acc->base.kind = KLEENE_REGEX_WORD;
    acc->word = word;
    return (kleene_regex *) acc;
}

kleene_regex* kleene_star( kleene_regex*  sub){
    kleene_regex_star* res = malloc(sizeof(kleene_regex_star));
    res->base.kind = KLEENE_REGEX_STAR;
    res->sub = sub;
    return (kleene_regex *)res;
}

kleene_regex* kleene_union(kleene_regex* left, kleene_regex* right){
    kleene_regex_union* res = malloc(sizeof(kleene_regex_union));
    res->base.kind = KLEENE_REGEX_UNION;
    res->left = left;
    res->right = right;
    return (kleene_regex *)res;
}

kleene_regex* kleene_concat(kleene_regex* left, kleene_regex* right){
    kleene_regex_concat* res = malloc(sizeof(kleene_regex_concat));
    res->base.kind = KLEENE_REGEX_CONCAT;
    res->left = left;
    res->right = right;
    return (kleene_regex *)res;
}

bool kleene_is_empty(kleene_regex* regex){
    if (regex->kind = KLEENE_REGEX_EMPTY){
        return true;
    }
    if(regex->kind == KLEENE_REGEX_EPSILON || regex->kind == KLEENE_REGEX_WORD || regex->kind == KLEENE_REGEX_STAR){
        return false;
    }
    if(regex->kind == KLEENE_REGEX_CONCAT){
        kleene_regex_concat* test = (kleene_regex_concat *) regex;
        return(kleene_empty(test->left) || kleene_empty(test->right));
    }
    if(regex->kind == KLEENE_REGEX_UNION){
        kleene_regex_union* test = (kleene_regex_union *) regex;
        return(kleene_empty(test->left) && kleene_empty(test->right));
    }
    return false; //On arrive jamais à ce cas normalment, mais on sait jamais (et on évite des warning)
}

bool kleene_has_epsilon(kleene_regex* regex){
    if (regex->kind = KLEENE_REGEX_EPSILON || regex->kind == KLEENE_REGEX_STAR){
        return true;
    }
    if(regex->kind == KLEENE_REGEX_EMPTY || regex->kind == KLEENE_REGEX_WORD){
        return false;
    }
    if(regex->kind == KLEENE_REGEX_CONCAT){
        kleene_regex_concat* test = (kleene_regex_concat *) regex;
        return(kleene_has_epsilon(test->left) && kleene_has_epsilon(test->right));
    }
    if(regex->kind == KLEENE_REGEX_UNION){
        kleene_regex_union* test = (kleene_regex_union *) regex;
        return(kleene_has_epsilon(test->left) || kleene_has_epsilon(test->right));
    }
    return false; //On arrive jamais à ce cas normalment, mais on sait jamais (et on évite des warning)
}






int main(void){
    char*  const word = "hello";
    kleene_regex_word* res = (kleene_regex_word *) kleene_word(word);
    printf("Word is: %s\n", res->word);
    return 0;
}


