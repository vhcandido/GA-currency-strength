#!/usr/bin/env python2

# +===========================================================================+
# | @author Victor Hugo Candido de Oliveira                                   |
# | 2016                                                                      |
# |                                                                           |
# | This is a simple Genetic Algorithm developed to optimize parameters of    |
# | a trading strategy based on Technical Analysis and currency strength.     |
# +===========================================================================+

from genetic import Population, Chromo
import sys
import util


def main(filename=None, port=1010):
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
                'tour_size': 8,
                'local': None
                }
    else:
        print 'Loading parameters from %s\n' % (filename)
        params = util.parse_json(filename)

    # Creating initial population
    pop = Population( size = params['size'],
            crossover = params['crossover'],
            mutation = params['mutation'],
            elitism = params['elitism'],
            imigration = params['imigration'],
            tour_size = params['tour_size'],
            local = params['local'])

    sock = util.open_socket(int(port))
    Chromo.sock = sock
    Chromo.cross_op = params['cross_op']

    # Generations without improvements
    no_improv = 0
    for i in range(params['max_generations']):
        print 'Generation %d' % (i+1)

        print 'Calculating fitness'
        util.send_msg(sock, 'NEWGEN\n')
        best = pop.evaluate()
        util.send_msg(sock, 'ENDGEN\n')
        print 'main:current_best ', best
        #pop.plot_evolution()

        if not pop.improved:
            no_improvements += 1
            print "Didn't improve:", no_improvements
            if no_improvements == params['max_not_improved']:
                print 'Reached limit for not improved generations'
                break
        else:
            no_improvements = 0

        print 'Evolving'
        pop.evolve()
        print

    print '\nBest solution:'
    #pop.show_first()
    print 'Fitness: ',best[0]
    print 'Genes: ', Chromo.to_str(best[1]), '\''

    # Closing connection
    util.send_msg(sock, 'ENDGA\n')
    sock.close()

if __name__ == '__main__':
    if len(sys.argv) > 2:
        main(sys.argv[1], sys.argv[2])
    else:
        print("\nInput file and comunication port were expected.\nExiting...\n")
        exit(1)





