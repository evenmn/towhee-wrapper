import os


@staticmethod
def read(filename='towhee_input'):
    # from __init__ import Towhee

    parameters = {}
    with open(filename, 'r') as f:
        for line in f:
            if line[0].islower():
                # keyword line
                if 'keyword' in locals():
                    parameters[keyword] = parameter
                parameter = ""
                keyword = line.split()[0]
                print(keyword)
            else:
                # parameter line
                parameter += line
                # print(parameter)
    print(parameters)


def rec_write(f, parameters):
    """Recursive function that allows an arbitrary number
    of parameter levels
    """
    level = 0
    for key, obj in parameters.items():
        user_value = obj.value
        default_value = obj.default
        if user_value != default_value:
            f.write(level * "\t" + key + "\n")
            f.write(level * "\t" + str(obj) + "\n")
            rec_write(f, obj.subparameters)


def write_input(self, filename='towhee_input'):
    """Write Towhee input script
    """
    filepath = os.path.join(self.wd, filename)

    with open(filepath, 'w') as f:
        rec_write(f, self.parameters)


def write_parallel(self, num_procs, filename="towhee_parallel"):
    """Write Towhee parallel script
    """
    filepath = os.path.join(self.wd, filename)

    with open(filepath, 'w') as f:
        f.write("\n")


if __name__ == "__main__":
    towhee = read("../data/towhee_input")
