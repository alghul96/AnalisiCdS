# Making a function OR
import numpy as np

class OR(object):
    def __init__(self):
        pass

    def OddsRatio(self, model_result):
        "Computes odds ratios starting from a logistic model"
        conf = model_result.conf_int()
        conf["OR"] = model_result.params
        conf.columns = ['2.5%', '97.5%', 'OR']
        self.oddstable = np.exp(conf).loc[:, ["2.5%", "OR", "97.5%"]]
        print self.oddstable
        return self.oddstable
