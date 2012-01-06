import numpy as np

from abundance import FractionalAbundance


class CoronalEquilibrium(object):
    def __init__(self, atomic_data):
        self.atomic_data = atomic_data
        self.ionisation_coeff = atomic_data.coeffs['ionisation']
        self.recombination_coeff = atomic_data.coeffs['recombination']
        self.nuclear_charge = atomic_data.nuclear_charge

    def ionisation_stage_distribution(self, temperature, density):
        y = self._compute_fractional_abundance(temperature, density)
        return FractionalAbundance(self.atomic_data, y, temperature, density)

    def _compute_fractional_abundance(self, temperature, density):
        y = np.zeros((self.nuclear_charge + 1, len(temperature)))
        y[0] = np.ones_like(temperature)
        for k in xrange(self.nuclear_charge):
            S = self.ionisation_coeff(k, temperature, density)
            alpha = self.recombination_coeff(k, temperature, density)
            y[k+1] = y[k] * S / alpha

        y /= y.sum(0) # fractional abundance
        return y


class ParallelCoronalEquilibrium(object):
    def __init__(self, atomic_data):
        from IPython.parallel import Client
        rc = Client()

        view = rc[:]
        view.block = True

        self.view = view
        self.atomic_data = atomic_data

    def ionisation_stage_distribution(self, temperature, density):
        temperature, density = np.broadcast_arrays(temperature, density)

        view = self.view
        view.scatter('temperature', temperature)
        view.scatter('density', density)
        view['eq'] = atomic.CoronalEquilibrium(self.atomic_data)

        view.execute(('y = eq._compute_fractional_abundance(temperature,density)'))

        y = np.hstack(view['y'])
        return FractionalAbundance(self.atomic_data, y, temperature,
                density)


if __name__ == '__main__':
    import atomic

    eq = ParallelCoronalEquilibrium(atomic.element('argon'))

    density = 1e19
    temperature = np.logspace(0, 4, 5000)
    y = eq.ionisation_stage_distribution(temperature, density)
    #y_serial = eq_serial.ionisation_stage_distribution(temperature, density)
