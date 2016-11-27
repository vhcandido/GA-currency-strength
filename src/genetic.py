import random

class Chromo(object):
    conn = None
    @staticmethod
    def generate_genes(size = None):
        if not size:
            size = 21*11 + 7 + 2
        return [ random.randint(1,100) for i in range(size) ]

    @staticmethod
    def fitness_calc(chromo):
        msg = Chromo.to_str(chromo) + '\n'
        Chromo.sock.send( msg.encode() )

        fitness = Chromo.sock.recv(5000)
        return float(fitness)

    @staticmethod
    def crossover((parent1, parent2)):
        ch1, ch2 = parent1[1], parent2[1]

        cross = random.randrange(3)
        # 0 - linear combination
        # 1 - one point
        # 2 - two point
        if cross == 0:
            a = random.random()
            for i in range(len(ch1)):
                x, y  = a*ch1[i] + (1-a)*ch2[i],\
                        a*ch2[i] + (1-a)*ch1[i]
                ch1[i], ch2[i] = int(round(x)), int(round(y))
        elif cross == 1:
            i = random.randint(1, len(ch1)-2)
            ch1[i:], ch2[i:] = ch2[i:], ch1[i:]
        elif cross == 2:
            i1 = random.randint(0, len(ch1))
            i2 = random.randint(0, len(ch1))
            if i1 > i2:
                i1, i2 = i2, i1
            ch1[i1:i2], ch2[i1:i2] = ch2[i1:i2], ch1[i1:i2]

        return [(0,ch1), (0,ch2)]

    @staticmethod
    def mutate(chromo):
        # Sort mutation factor in (-5,5)
        # Apply this mutation factor to randomly sampled genes
        # These genes represent 25% (1/4) of total genes
        ch = chromo[1]
        l = len(ch)
        m = random.randint(-5,5)
        for i in random.sample(range(l), int(l/4)):
            # Check if its within limits (6, 95)
            ch[i] += m if 5 < ch[i] < 96 else 0
        return (0, ch)

    @staticmethod
    def to_str(chromo):
        return ','.join(str(gene) for gene in chromo[1])

class Population(object):
    def __init__(self,
            size = 100,
            crossover = 0.3,
            mutation = 0.05,
            elitism = 0.2,
            imigration = 0.2,
            tour_size = 8):

        self.size = size
        self.crossover = crossover
        self.mutation = mutation
        self.elitism = elitism
        self.imigration = imigration
        self.tour_size = tour_size
        self.population = [ (0.0, Chromo.generate_genes()) for i in range(size) ]

        self.improved = False
        self.cur_best = (-10000, None)

    def tournament_selection(self):
        return max(random.sample(self.population, self.tour_size))

    def select_parents(self):
        parent1 = self.tournament_selection()
        parent2 = self.tournament_selection()
        return [parent1, parent2]

    def evaluate(self):
        self.population = sorted(\
                [ (Chromo.fitness_calc(ch), ch) for ch in self.population ],\
                reverse = True)

        # Check if it has improved
        new_best = self.population[0]
        self.improved = new_best[0] > self.cur_best[0]
        return new_best if self.improved else self.cur_best

    def evolve(self):
        # Elitism
        idx = int(self.elitism * self.size)
        next_pop = self.population[:idx]

        # Imigration
        for i in range(int(self.imigration * self.size)):
            next_pop.append(Chromo.generate_genes())

        # Crossover and mutation
        while len(next_pop) < size:
            parents = self.select_parents()
            cross = random.random() < self.crossover
            childs = Chromo.crossover(parents) if cross else parents
            for ch in childs:
                mutate = random.random() < self.mutate
                next_pop.append(Chromo.mutate(ch) if mutate else ch)

        # Save the next generation and evaluate each individual's fitness
        self.population = next_pop[:self.size]

#
#
#
#
#
