"""
TRexSelector: T-Rex Selector for High-Dimensional Variable Selection & FDR Control

This package performs fast variable selection in high-dimensional settings while 
controlling the false discovery rate (FDR) at a user-defined target level.
"""

from .trex import trex
from .random_experiments import random_experiments
from .lm_dummy import lm_dummy
from .screen_trex import screen_trex
from .add_dummies import add_dummies
from .add_dummies_GVS import add_dummies_GVS
from .FDP import FDP
from .TPP import TPP
from .fdp_hat import fdp_hat
from .select_var_fun import select_var_fun
from .select_var_fun_DA_BT import select_var_fun_DA_BT
from .phi_prime_fun import Phi_prime_fun
from .gauss_data import generate_gaussian_data

__version__ = '0.6.15'
