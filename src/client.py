#!/usr/bin/env python2

# +===========================================================================+
# | @author Victor Hugo Candido de Oliveira                                   |
# | 2016                                                                      |
# |                                                                           |
# | This is a simple Genetic Algorithm developed to optimize parameters of    |
# | a trading strategy based on Technical Analysis and currency strength.     |
# +===========================================================================+

from genetic import Population
import sys
import json

def parse_json(filename):
    with open(filename) as data_file:
            data = json.load(data_file)
    return data

def main(filename=None):
    if not filename:
        print 'Using default parameters'
        params = {
                'max_generations': 200,
                'max_not_improved': 20,
                'size': 100,
                'crossover': 0.3,
                'mutation': 0.05,
                'elitism': 0.2,
                'imigration': 0.2,
                'tour_size' = 8,
                'port': 1010
                }
    else:
        print 'Loading parameters from %s' % (filename)
        params = parse_json(filename)

    # Creating initial population
    pop = Population( size = params['size'],
            crossover = params['crossover'],
            mutation = params['mutation'],
            elitism = params['elitism'],
            imigration = params['imigration'],
            tour_size = params['tour_size'],
            port = params['port'] )

    # Generations without improvements
    no_improv = 0
    for i in range(params['max_generations']):
        print 'Generation %d' % (i+1)

        print 'Calculating fitness'
        best = pop.evaluate()
        #pop.plot_evolution()

        if not pop.improved:
            no_improvements += 1
            print "Didn't improve:", no_improvements
            if no_improvements == max_not_improved:
                print 'Reached limit for not improved generations'
                break
        else:
            no_improvements = 0

        print 'Evolving'
        pop.evolve()
        print

    print 'Best solution:'
    #pop.show_first()
    print best[0], Chromo.to_str(best)

if __name__ == '__main__':
    if len(sys.argv) > 1:
        main(sys.argv[1])
    else:
        print("\nInput file was expected.\nExiting...\n")
        exit(1)





