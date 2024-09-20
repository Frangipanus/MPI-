#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "tsp_approx.h"

int main(int argc, char *argv[])
{
	int poids_graphe_exemple[] = {
		0, 1, 10, 5, 4,
		1, 0, 6, 12, 7,
		10, 6, 0, 2, 3,
		5, 12, 2, 0, 9,
		4, 7, 3, 9, 0
	};
	graphe_complet_pondere graphe_exemple = {
		.sommets = 5,
		.poids = poids_graphe_exemple
	};
	assert(graphe_exemple.sommets * graphe_exemple.sommets == sizeof(poids_graphe_exemple)/sizeof(int));

	if(argc < 3) {
		fprintf(stderr, "Usage: %s sommet_u sommet_v\n", argv[0]);
		return -1;
	}

	int u = strtol(argv[1], NULL, 10);
	int v = strtol(argv[2], NULL, 10);
	if(u >= graphe_exemple.sommets) {
		fprintf(stderr,
			"Le sommet_u est hors du graphe (%d >= %d)\n",
			u,
			graphe_exemple.sommets);
		return -2;
	}
	if(v >= graphe_exemple.sommets) {
		fprintf(stderr,
			"Le sommet_v est hors du graphe (%d >= %d)\n",
			v,
			graphe_exemple.sommets);
		return -2;
	}

	printf("L'arête {%d, %d} a pour poids %d\n", u, v, poids_arete(&graphe_exemple, u, v));

	return 0;
}
