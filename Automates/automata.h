#ifndef __AUTOMATA_H
#define __AUTOMATA_H

#include <stdbool.h>
#include <stdio.h>
/*Ceci est un commentraire*/
/* tag::type_automaton[] */
struct transition_s {
	int src_state;
	int dest_state;
};
typedef struct transition_s transition;

struct automaton_s {
	int nb_states;
	bool *is_final;
	int first_transition[1 + 256 * sizeof(unsigned char)];
	transition *transitions;
};
typedef struct automaton_s automaton;
/* end::type_automaton[] */

bool automaton_accepts(const automaton *automaton, unsigned const char *word);
automaton* automaton_read(FILE *fh);
void automaton_free(automaton *automaton);
bool automaton_nd_accepts(const automaton *automaton, unsigned const char *word);
bool automaton_nd_accepts_parallel(const automaton *automaton, unsigned const char *word);

#endif
