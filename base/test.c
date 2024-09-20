#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

int  main(void){
    int* test = (int*)malloc(sizeof(int));
    test[0] = 42;
    int* acc = test;
    free(test);
    char* c = "a";
    if(c[1] == '\0'){
        printf("ouf");
    }
    return 0;
}