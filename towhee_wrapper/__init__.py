import os
import shutil
from .io import read


class Towhee:
    def __init__(self, ensemble, temperature):

        # from .default import keywords, default_parameters
        from .parameter import Parameter
        from .io import write, write_parallel
        from .config import *

        self.ensemble = ensemble
        self.temperature = temperature
        self.boxes = []
        self.wd = ""

        self.keywords = keywords
        self.default_parameters = default_parameters
        self.parameters = default_parameters

    def set_working_directory(self, wd, overwrite=False):
        self.wd = wd
        if overwrite:
            self.wd = wd
            try:
                os.makedirs(wd)
            except FileExistsError:
                pass
        else:
            ext = 0
            repeat = True
            while repeat:
                try:
                    os.makedirs(self.wd)
                    repeat = False
                except FileExistsError:
                    ext += 1
                    self.wd = wd + f"_{ext}"
        self.wd += "/"

    def copy_to_wd(self, *filename):
        """Copy one or several files to working directory.

        :param filename: filename or list of filenames to copy
        :type filename: str or list of str
        """

        for file in filename:
            head, tail = os.path.split(file)
            shutil.copyfile(file, self.wd + tail)

    def run(self, num_procs, towhee_exec='towhee'):
        """Run
        """
        self.write()
        self.write_parallel()
