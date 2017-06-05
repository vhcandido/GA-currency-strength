from copy import deepcopy
import random
from Queue import Queue
from threading import Thread
from multiprocessing import Process, Array

class Chromo(object):
    conn = None
    cross_op = -1
    minv = list()
    maxv = list()

    @staticmethod
    def generate_genes(size = 240):
        return [ random.randint(Chromo.minv[i],Chromo.maxv[i]) for i in range(size) ]

    @staticmethod
    def set_intervals(size = 240):
        for i in range(size):
            min_, max_ = Chromo.get_intervals(i)
            Chromo.minv.append(min_)
            Chromo.maxv.append(max_)

    @staticmethod
    def get_intervals(gpos):
        maxv = 100
        minv = 1
        if gpos == 0:
            maxv = 5
        elif gpos == 1:
            maxv = 10
        elif 44 <= gpos < 65:
            # [2*21 + 2, 3*21 + 2)
            minv = 0
            maxv = 23
        elif 65 <= gpos < 72:
            #
            minv = 0
            maxv = 11
        elif 219 <= gpos < 240:
            maxv = 5

        return (minv, maxv)


    @staticmethod
    def fitness_helper(genes, sock):
        msg = Chromo.to_str(genes) + '\n'
        sock.send( msg.encode() )

        fitness = sock.recv(5000)
        return float(fitness)

    @staticmethod
    def fitness_calc(sock, queue, fitness):
        print 'Thread started...'
        while not queue.empty():
            i, genes = queue.get()
            fit = Chromo.fitness_helper(genes, sock)
            print 'fitness_calc: [%d, %f]' % (i, fit)
            fitness[i] = fit
            queue.task_done()
        print 'Thread finishing...'

    @staticmethod
    def crossover((parent1, parent2)):
        ch1, ch2 = parent1, parent2

        if Chromo.cross_op == -1:
            cross = random.randrange(3)
        else:
            cross = Chromo.cross_op
        # 0 - linear combination
        # 1 - one point
        # 2 - two point
        print cross,
        if cross == 0:
            a = random.random()
            for i in range(len(ch1)):
                x, y  = a*ch1[i] + (1-a)*ch2[i],\
                        a*ch2[i] + (1-a)*ch1[i]
                ch1[i], ch2[i] = int(round(x)), int(round(y))
        elif cross == 1:
            i = random.randint(0, len(ch1))
            ch1[i:], ch2[i:] = ch2[i:], ch1[i:]
        elif cross == 2:
            i1 = random.randint(0, len(ch1))
            i2 = random.randint(0, len(ch1))
            if i1 > i2:
                i1, i2 = i2, i1
            ch1[i1:i2], ch2[i1:i2] = ch2[i1:i2], ch1[i1:i2]

        return (ch1, ch2)

    @staticmethod
    def mutate(ch):
        # Sort mutation factor in (-5,5)
        # Apply this mutation factor to randomly sampled genes
        # These genes represent 25% (1/4) of total genes
        l = len(ch)
        for i in random.sample(range(l), int(l/10)):
            m = random.randint(-5,5)
            # Check if its within limits [1, 100]
            ch[i] = max(Chromo.minv[i], min(Chromo.maxv[i], ch[i] + m))
            #ch[i] += m if Chromo.minv[i] <= ch[i]+m <= Chromo.maxv[i] else 0
        return ch

    @staticmethod
    def to_str(genes):
        return ','.join(str(gene) for gene in genes)

class Population(object):
    def __init__(self,
            size = 100,
            crossover = 0.3,
            mutation = 0.05,
            elitism = 0.2,
            imigration = 0.2,
            tour_size = 8,
            local = None):

        self.size = size
        self.crossover = crossover
        self.mutation = mutation
        self.elitism = elitism
        self.imigration = imigration
        self.tour_size = tour_size
        self.fitness = [0.0] * size
        self.chromo_history = {-1:None}

        # Create lists with intervals for each gene
        Chromo.set_intervals()

        if not local:
            print 'Creating population randomly'
            self.population = [ Chromo.generate_genes() for i in range(size) ]
        else:
            print 'Creating population based on local search'
            self.population = [ Chromo.mutate(deepcopy(local)) for i in range(size) ]
            self.population[0] = local

        self.improved = False
        self.cur_best = (-10000, None)

    def tournament_selection(self):
        # Sample the index of the tournament candidates
        candidates = random.sample(range(self.size), self.tour_size)

        # Return the index of the best candidate
        return max(candidates, key = lambda i: self.fitness[i])

    def select_parents(self):
        parent1 = self.population[self.tournament_selection()]
        parent2 = self.population[self.tournament_selection()]
        return (parent1, parent2)

    def evaluate(self):
        q = Queue()
        a = Array('d', [0.0] * self.size)
        for i,ch in enumerate(self.population):
            ch_str = Chromo.to_str(ch)
            if ch_str in self.chromo_history:
                fit = self.chromo_history[ch_str]
                a[i] = fit
                print 'evaluate:old_fitness:%d %f' % (i,fit)
            else:
                print 'evaluate:adding_to_queue:%d' % i
                q.put((i, self.population[i]))
        threads = []
        for sock in Chromo.sockets:
            t = Thread(target=Chromo.fitness_calc, args=(sock, q, a))
            t.daemon = True
            t.start()
            threads.append(t)
        # Wait for threads to finish
        for t in threads:
            t.join()

        self.fitness = list(a)
        sorted_idx = sorted(range(self.size), key=lambda i: self.fitness[i], reverse=True)
        self.fitness = [self.fitness[i] for i in sorted_idx]
        self.population = [self.population[i] for i in sorted_idx]

        # Adding chromosomes to history
        for i, ch in enumerate(self.population):
            ch_str = Chromo.to_str(ch)
            if ch_str not in self.chromo_history:
                self.chromo_history[ch_str] = self.fitness[i]

        # Check if it has improved
        best_candidate = (self.fitness[0], self.population[0])
        self.improved = best_candidate[0] > self.cur_best[0]
        self.cur_best = best_candidate if self.improved else self.cur_best
        return self.cur_best

    def selection(self):
        par_list = list()
        while 2*len(par_list) < self.size:
            # Deepcopy the parent tuple
            # otherwise parents and childs will point to the same object (!)
            parents = deepcopy(self.select_parents())
            par_list.append(parents)
        return par_list

    def evolve(self):
        # Elitism
        eli = int(self.elitism * self.size)
        print 'Elitism - saving %d chromo' % (eli)
        next_pop = list(self.population[:eli])

        # Imigration
        imi = int(self.imigration * self.size)
        print 'Imigration - importing %d chromo' % (imi)
        for i in range(imi):
            next_pop.append(Chromo.generate_genes())

        # Selection
        par_list = self.selection()

        # Crossover and mutation
        print 'X\tM1\tM2'
        for parents in par_list:
            childs = ()

            cross = random.random() < self.crossover
            if cross:
                print 'Y',
                childs = Chromo.crossover(parents)
            else:
                print 'N',
                childs = parents

            for ch in childs:
                mutate = random.random() < self.mutation
                if mutate:
                    print '\tY',
                    ch = Chromo.mutate(ch)
                else:
                    print '\tN',
                next_pop.append(ch)
            print

        # Save the next generation and evaluate each individual's fitness
        self.population = next_pop[:self.size]

#
#
#
#
#
