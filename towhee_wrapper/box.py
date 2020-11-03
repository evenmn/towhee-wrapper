import numpy as np


class Dimensions:
    def __init__(self, initstyle, initmol, hmatrix, initlattice=None, ini=None):
        self.initmol = int(initmol)
        if initlattice is None:
            if initstyle == 'coords':
                self.initlattice = 'none'
            else:
                self.initlattice = 'simple cubic'

        supported_initstyles = ['full cbmc', 'coords', 'helix cbmc',
                                'nanotube', 'template', 'partial cbmc']
        supported_initlattices = ['center', 'none', 'simple cubic']

        if initstyle in supported_initstyles:
            self.initstyle = initstyle
        else:
            raise NotImplementedError(
                f"Initial style {initstyle} is not implemented")

        if initlattice in supported_initlattices:
            self.initlattice = initlattice
        else:
            raise NotImplementedError(
                f"Initial lattice {initlattice} is not implemented")

        hmatrix = np.asarray(hmatrix)
        shape = hmatrix.shape
        assert shape == (3, 3), f"Expected hmatrix dimensions (3, 3), got {shape}"
        self.hmatrix = hmatrix

        if ini is None:
            mol_per_axis = self.initmol**(1/3)
            self.ini = 3 * [int(mol_per_axis)]
        else:
            assert len(ini) == 3, f"Expected ini length 3, but {len(ini)} was given"
            self.ini = ini

    def __repr__(self):
        return 'dimensions'


class NumberDensity:
    def __init__(self, initstyle, initlattice, initmol, number_density, ini=None):
        self.initmol = int(initmol)
        if initlattice is None:
            if initstyle == 'coords':
                self.initlattice = 'none'
            else:
                self.initlattice = 'simple cubic'

        supported_initstyles = ['full cbmc', 'coords', 'helix cbmc',
                                'nanotube', 'template', 'partial cbmc']
        supported_initlattices = ['center', 'none', 'simple cubic']

        if initstyle in supported_initstyles:
            self.initstyle = initstyle
        else:
            raise NotImplementedError(
                f"Initial style {initstyle} is not implemented")

        if initlattice in supported_initlattices:
            self.initlattice = initlattice
        else:
            raise NotImplementedError(
                f"Initial lattice {initlattice} is not implemented")

        self.number_density = float(number_density)

        if ini is None:
            mol_per_axis = self.initmol**(1/3)
            self.ini = 3 * [int(mol_per_axis)]
        else:
            assert len(ini) == 3, f"Expected ini length 3, but {len(ini)} was given"
            self.ini = ini

    def __repr__(self):
        return 'number density'


class UnitCell:
    def __init__(self, inix, iniy, iniz):
        self.inix = int(inix)
        self.iniy = int(iniy)
        self.iniz = int(iniz)

    def __repr__(self):
        return 'unit cell'
