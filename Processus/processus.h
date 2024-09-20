#include<pthread.h>
#include<stdlib.h>
#include<stdbool.h>
#include<string.h>

struct deque_s{
    void** data;
    int capacity;
    int top;
    int botom;
    pthread_mutex_t top_lock;
};
typedef struct deque_s deque;

struct argument{
    int* tab; 
    int taille;
    int ind;
    int nb_action_ind;
    int* nb_action;
    bool* a_agi;
    int* phase;
};
typedef struct argument argument;

struct quick{
    int* tab;
    int debut;
    int fin; //On s'arrete a fin-1 en fait
};
typedef struct quick quick;
struct quick2{
    int* tab;
    int* acc; 
    int debut; 
    int fin; //On s'arrete a fin -1
    int nb_process;
    int* avant;
    int* apres;
    int* pivot;
    bool* pivot_found;
    int* ind_pivot;
};
typedef struct quick2 quick2;

