class Wrapper:
    def __init__(self, ensemble, temperature):

        from .io import read, write
        from .config import *

        self.ensemble = ensemble
        self.temperature = temperature
        self.boxes = []
