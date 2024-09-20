.PHONY: all

all: test_poids_tour test_poids_arete

test_poids_arete: test_poids_arete.o tsp_approx.o
	gcc -o $@ $^

test_poids_tour: test_poids_tour.o tsp_approx.o
	gcc -o $@ $^


	

%.o: %.c
	gcc -Wall -c -o $@ $<
