#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include<assert.h>
#include<stdbool.h>
#include<pthread.h>

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


int acc;
int* compteur = &acc;
pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t actions1 = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t actions2 = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t all_unlocked = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t go = PTHREAD_MUTEX_INITIALIZER;


void* fonc(void* v){
    printf("hello world\n");
    return v;
}

void* incremente(void* v){
    for(int i=0; i<10000; i++){
        pthread_mutex_lock(&mutex);
        compteur[0] = compteur[0]+1;
        pthread_mutex_unlock(&mutex);
        
    }
    return v;
}

void* lutins(void* v_untyped){
    
    argument* v = v_untyped;
    int i = v->ind;
    
    while(v->nb_action_ind < v->taille){
        if(v->phase[0] % 2 == (i % 2) && !(v->a_agi[0])){
            if(v->tab[i]>v->tab[i+1]){
                int acc = v->tab[i];
                v->tab[i] = v->tab[i+1];
                v->tab[i+1] = acc;
            }
            v->nb_action_ind = v->nb_action_ind + 1;
            if(i%2==0){
                pthread_mutex_lock(&actions1);
            v->nb_action[0] = v->nb_action[0]+1;
            v->a_agi[0]= true;
            pthread_mutex_unlock(&actions1);
            }
            else{
                pthread_mutex_lock(&actions2);
            v->nb_action[0] = v->nb_action[0]+1;
            v->a_agi[0]= true;
            pthread_mutex_unlock(&actions2);
            }
            
            
        }
    }
    return v_untyped;
    
}




void tri_bulle(int length, int* tableau){
    pthread_t* tthreadtab = (pthread_t*)malloc(length*sizeof(pthread_t));
    argument* tabarg = (argument*)malloc(length*sizeof(argument));
    int* action = (int*)malloc((sizeof(int)));
    int* action1 = (int*)malloc((sizeof(int)));
    int* action2 = (int*)malloc((sizeof(int)));
    int* phase = (int*)malloc((sizeof(int)));
    bool** vas_y = (bool**)malloc(length*sizeof(bool*));
    for(int j = 0; j < length; j ++){
        bool* acc = (bool*)malloc(sizeof(bool));
        vas_y[j] = acc;
    }
    action[0]=0;
    action1[0]=0;
    action2[0]=0;
    phase[0]=0;
    for(int j=0; j<length-1; j++){
        vas_y[j][0] = false;
        tabarg[j].ind = j;
        tabarg[j].tab = tableau;
        tabarg[j].taille = length;
        tabarg[j].nb_action_ind = 0;
        if(j%2 == 0){
            tabarg[j].nb_action = action1;
        }
        else{
            tabarg[j].nb_action = action2;
        }
        tabarg[j].phase = phase;
        tabarg[j].a_agi = vas_y[j];
    }
    
    for(int i = 0; i < length-1; i++){
        void* v = &tabarg[i];
        pthread_create(&tthreadtab[i], NULL, lutins, v);        
    } 
    while(phase[0]< 2*length){
        //printf("On est à la phase%d et %d  %d\n", phase[0], tabarg[1].nb_action_ind, action2[0]);
        if(phase[0]%2 == 0){
            while(action1[0]<length/2){
                ;
            }
            phase[0] = phase[0] + 1;
            pthread_mutex_lock(&actions1);
            action1[0] = 0;
            for(int j = 0; j < length-1; j = j + 2){
                tabarg[j].a_agi[0]= false;
            }
            pthread_mutex_unlock(&actions1);
        }
        else{
            
            
          while(action2[0]<(length-1)/2 ){
            
                ;
            }
            phase[0] = phase[0] + 1;  
            pthread_mutex_lock(&actions2);
            action2[0] = 0;
            for(int j = 1; j < length-1; j = j + 2){
                tabarg[j].a_agi[0]= false;
            }
            pthread_mutex_unlock(&actions2);
        }

    }
    for(int i = 0; i < length-1; i++){
        pthread_join(tthreadtab[i], NULL);
    }
    
    
}



int main(void){
    srand(time(NULL));
    pthread_t thread1;
    pthread_t thread2;
    pthread_create(&thread1, NULL, fonc, NULL);
    pthread_create(&thread2, NULL, fonc, NULL);
    pthread_join(thread1, NULL);
    pthread_join(thread2, NULL);
	pthread_t Alice;
    pthread_t Bob;
    pthread_create(&Alice, NULL, incremente, NULL);
    pthread_create(&Bob, NULL, incremente, NULL);
    pthread_join(Alice, NULL);
    pthread_join(Bob, NULL);
    printf("On obtient %d\n", compteur[0]);
    int* tableau = (int*)malloc(10*sizeof(int));
    tableau[0] = 5;
    tableau[1] = 4;
    tableau[2] = 7;
    tableau[3] = 8;
    tableau[4] = 15;
    tableau[5] = 3;
    tableau[6] = 9;
    tableau[7] = 12;
    tableau[8] = 75;
    tableau[9] = 1;
    tri_bulle(10,tableau);
    for(int i = 0; i < 10; i++){
        printf("%d\n", tableau[i]);
    }
    printf("----------------------------------------------------------------\n");
    int* tableau2 = (int*)malloc(50*sizeof(int));
    for(int i = 0; i <50; i++){
        tableau2[i] = rand()%1000;
    }
    printf("End géné\n");
    int min = tableau2[0];
    
    tri_bulle(50,tableau2);
    printf("min = %d", min);
    for(int i = 0; i < 10; i++){
        printf("%d\n", tableau2[i]);
    }
    
    


    return 0;
}