// ========================= START PRELUDE ===================== //
#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

// Utilitaires

#define MAX_MOTIF_LEN 1000

// Le type des chaines null-terminated contenant ACGT.
typedef char* string;

// Un vecteur de string
struct vector_s {
  int len;
  string * strs;
};
typedef struct vector_s vector;

// `allocate_vector()` aloue un vecteur vide
vector* allocate_vector() {
  string *all_lines = malloc(MAX_MOTIF_LEN * sizeof(string));
  for(int i=0;i<MAX_MOTIF_LEN;i++) { all_lines[i] = NULL; };
  vector* v = malloc(sizeof(vector));
  v->len = 0;
  v->strs = all_lines;
  return v;
}

// `add_string(v,s)` ajoute la string `s` au vecteur `v`.
void add_string(vector* v, string s) {
  v->strs[v->len] = s;
  v->len = v->len +1;
}

// `read_text(filename)` lit la chaine dans le fichier `filename` et renvoi
// une string
string read_text(char* filename) {
  FILE *fptr = fopen(filename, "r");
  
  char * line = NULL;
  size_t len = 0;
  int read = getline(&line, &len, fptr);
  if (read != -1) { return line; } { return NULL; }
}

// `read_motif(filename)` lit le motif dans le fichier `filename` et renvoi
// un vector
vector* read_motif(char* filename) {
  FILE *fptr = fopen(filename, "r");

  vector* m = allocate_vector();

  string line = NULL;  
  size_t len = 0;
  ssize_t read;

  while ((read = getline(&line, &len, fptr)) != -1) {
    if (line[read-1] == '\n') { line[read-1] = '\0'; };
    add_string(m, line);
    line = NULL;
  }
  return m ;
}

// `string_of_node(depth, pos, repr, a, c, g, t)` converti le noeud d'un patricia trie fourni en une string à l'indentation `depth`, étant donné les informations `pos` et `repr` et les strings représentant les descendants a, c, g, t
string string_of_node(int depth, unsigned pos, string repr,
                      string a, string c, string g, string t) {
  string res = malloc(8*1000);
  sprintf(res, "pos=%d repr=%s\n", pos, repr);
  char blank[depth+1];
  for (int i = 0;i<depth;i++) { blank[i]=' ';};
  blank[depth]='\0';
  if (strcmp(a,"")!=0) { strcat(res, blank); strcat(res, "└A→ "); strcat(res, a);};
  if (strcmp(c,"")!=0) { strcat(res, blank); strcat(res, "└C→ "); strcat(res, c);};
  if (strcmp(g,"")!=0) { strcat(res, blank); strcat(res, "└G→ "); strcat(res, g);};
  if (strcmp(t,"")!=0) { strcat(res, blank); strcat(res, "└T→ "); strcat(res, t);};
  return res;
}

// ========================= STOP PRELUDE ===================== //

struct patricia_t{
  int pos;
  char* repr;
  struct patricia_t * a;
  struct patricia_t * c;
  struct patricia_t * g;
  struct patricia_t * t;
};
typedef struct patricia_t patricia;

char* plus_grand_préfixe(vector* v, int* longeur){

  int len = 0;
  char* préfixe = (char*)malloc((strlen(v->strs[0]+1))*sizeof(char));
  bool done = false;
  while (!done){

    
    préfixe[len] = 'A';

    for(int i = 0; i < v->len; i++){
      if(v->strs[i][len] != 'A'){
        done = true;
       
        break;
      }
    }
    if(!done){
      len = len + 1;
      continue;
    }
    done = false;
    préfixe[len] = 'C';
    for(int i = 0; i < v->len; i++){
      if(v->strs[i][len] != 'C'){
        done = true;
        break;
      }
    }
    if(!done){
      len = len + 1;
      continue;
    }
    done = false;
    préfixe[len] = 'G';
    for(int i = 0; i < v->len; i++){
      if(v->strs[i][len] != 'G'){
        done = true;
        break;
      }
    }
    if(!done){
      len = len + 1;
      continue;
    }
    done = false;
    préfixe[len] = 'T';
    for(int i = 0; i < v->len; i++){
      if(v->strs[i][len] != 'T'){
        done = true;
        break;
      }
    }
    if(!done){
      len = len + 1;
      continue;
    }
  }
  char* res = (char*)malloc((len+1)*sizeof(char));
  for(int i = 0; i < len; i ++){
    res[i] = préfixe[i];
  }
  free(préfixe);
  res[len] = '\0';
  longeur[0] = len;
 
  return res;
}


patricia* make_patricia(vector* v){
  patricia* res = (patricia*)malloc(sizeof(patricia));
  if(v->len == 1){
    res->pos = -1;
    res->repr = v->strs[0];
    res->a = NULL;
    res->c = NULL;
    res ->g = NULL;
    res->t = NULL;
    return res;
  }
  int* len = (int*)malloc(sizeof(int));
  char* prefixe = plus_grand_préfixe(v, len);
  res->pos = len[0];
  res->repr = NULL;
  for(int i = 0; i < v->len; i++){
    if(v->strs[i][len[0]] == '\0'){
      res->repr = v->strs[i];
    }
  }
  //Séparons en 4 vecteur selon la valeur du suivant

  vector* va = allocate_vector();
  for(int i = 0; i < v->len; i++){
    if(v->strs[i][len[0]] == 'A'){
      add_string(va, v->strs[i]);
    }
  }

  if(va->len == 0){

    res->a = NULL;
  }
  else{
   
    res->a = make_patricia(va);
  }
  
  vector* vc = allocate_vector();
  for(int i = 0; i < v->len; i++){
    if(v->strs[i][len[0]] == 'C'){
      add_string(vc, v->strs[i]);
    }
  }
  if(vc->len == 0){
    res->c = NULL;
  }
  else{
    res->c = make_patricia(vc);
  }

  vector* vg = allocate_vector();
  for(int i = 0; i < v->len; i++){
    if(v->strs[i][len[0]] == 'G'){
      add_string(vg, v->strs[i]);
    }
  }
  if(vg->len == 0){
    res->g = NULL;
  }
  else{
    res->g = make_patricia(vg);
  }


  vector* vt = allocate_vector();
  for(int i = 0; i < v->len; i++){
    if(v->strs[i][len[0]] == 'T'){
      add_string(vt, v->strs[i]);
    }
  }
  if(vt->len == 0){
    res->t = NULL;
  }
  else{
    res->t = make_patricia(vt);
  }
  return res;

}



int size(patricia* tree){
  if(tree == NULL){
    return 0;
  }
  else{
    return (1+size(tree->a)+ size(tree->c)+size(tree->g)+size(tree->t));
  }
}


bool is_prefixe(string t, string pre){
  if(pre == NULL){
    return false;
  }
  int i = 0;
  while( (pre[i]!='\0') && (pre[i]=t[i]) && (t[i]!='\0') ){
    i = i + 1;
  }
  return(pre[i]=='\0');
}

bool is_same_string(string t, char* pre){
  
  if(pre == NULL){
    return false;
  }
   int i = 0;
  while( (pre[i]!='\0') && (pre[i]=t[i]) && (t[i]!='\0') ){
    i = i + 1;
  }
  return((pre[i]=='\0') && (t[i]=='\0'));
}

int mem(string t, patricia* tree){
  int n = strlen(t);
  
  char* acc = tree->repr;
  
  if(is_prefixe(t, tree->repr)){

    if((tree->pos == n)  && (tree->repr != NULL) && is_same_string(t, tree->repr) ){
      return 1;
    }
  else if (tree->pos < n ){
    if(t[tree->pos]=='A'){
      return mem(t, tree->a);
    }
    if(t[tree->pos]=='C'){
      return mem(t, tree->c);
    }
    if(t[tree->pos]=='G'){
      return mem(t, tree->g);
    }
    if(t[tree->pos]=='T'){
      return mem(t, tree->t);
    }
    return 0;
    }
  }
  return 0;
}




int main(int argc, char *argv[]){
  printf("Question 6:\n");
  int*acc = (int*)malloc(sizeof(int));
  printf("%d\n", size(make_patricia(read_motif("motif_5.txt"))));
  //vector* test = allocate_vector();
  //add_string(test, "AAAG");
  //add_string(test, "AAA");
  //printf("%s, %s, %s\n", plus_grand_préfixe(test, acc), test->strs[0], test->strs[1]);
  vector* motif = read_motif("motif_10.txt");
  printf("%d\n", size(make_patricia(motif)));
  printf("%d\n", size(make_patricia(read_motif("motif_100.txt"))));
  printf("%d\n", size(make_patricia(read_motif("motif_1000.txt"))));
  printf("Question 7:\n");
  patricia* tree = make_patricia(read_motif("motif_10.txt"));
  
  printf("%d\n",  mem(read_text("chaine_2.txt"), tree));
  printf("%d\n",  mem(read_text("chaine_3.txt"), make_patricia(read_motif("motif_10.txt"))));


  return 0;
}
