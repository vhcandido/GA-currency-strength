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
