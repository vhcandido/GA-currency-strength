from copy import deepcopy
import random

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
    def fitness_calc(genes):
        msg = Chromo.to_str(genes) + '\n'
        Chromo.sock.send( msg.encode() )

        fitness = Chromo.sock.recv(5000)
        return float(fitness)

    @staticmethod
    def crossover((parent1, parent2)):
        ch1, ch2 = parent1[1], parent2[1]

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

        return [(0,ch1,True), (0,ch2,True)]

    @staticmethod
    def mutate(chromo):
        # Sort mutation factor in (-5,5)
        # Apply this mutation factor to randomly sampled genes
        # These genes represent 25% (1/4) of total genes
        ch = chromo[1]
        l = len(ch)
        for i in random.sample(range(l), int(l/4)):
            m = random.randint(-5,5)
            # Check if its within limits [1, 100]
            ch[i] += m if Chromo.minv[i] <= ch[i]+m <= Chromo.maxv[i] else 0
        return (0, ch, True)

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

        # Create lists with intervals for each gene
        Chromo.set_intervals()

        if not local:
            print 'Creating population randomly'
            self.population = [ (0.0, Chromo.generate_genes(), True) for i in range(size) ]
        else:
            print 'Creating population based on local search'
            self.population = [ Chromo.mutate((0, deepcopy(local), True)) for i in range(size) ]
            self.population[0] = (0, local, True)

        self.improved = False
        self.cur_best = (-10000, None)

    def tournament_selection(self):
        return max(random.sample(self.population, self.tour_size))

    def select_parents(self):
        parent1 = self.tournament_selection()
        parent2 = self.tournament_selection()
        return [parent1, parent2]

    def evaluate(self):
        eval_pop = list()
        for ch in self.population:
            if ch[2]:
                fitness = Chromo.fitness_calc(ch[1])
                print "evaluate:new_fitness ", fitness
                eval_pop.append( (fitness, ch[1], False) )
            else:
                print 'evaluate:old_fitness ', ch[0]
                eval_pop.append( ch )
        self.population = sorted(eval_pop, reverse = True)

        # Check if it has improved
        new_best = self.population[0]
        self.improved = new_best[0] > self.cur_best[0]
        self.cur_best = new_best if self.improved else self.cur_best
        return new_best if self.improved else self.cur_best

    def evolve(self):
        # Elitism
        eli = int(self.elitism * self.size)
        print 'Elitism - saving %d chromo' % (eli)
        next_pop = list(self.population[:eli])

        # Imigration
        imi = int(self.imigration * self.size)
        print 'Imigration - importing %d chromo' % (imi)
        for i in range(imi):
            next_pop.append((0, Chromo.generate_genes(), True))

        # Selection
        par_list = list()
        print 'Selecting parents'
        while 2*len(par_list) < self.size:
            # Deepcopy the parent tuple
            # otherwise parents and childs will point to the same object (!)
            parents = deepcopy(self.select_parents())
            par_list.append(parents)

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
