from igraph import *
import pandas as pd

def feedback_set(dat):
    edgelist = list(dat[["Tail", "Head"]].itertuples(index=False, name = None))
    net = Graph.TupleList(edges = edgelist, directed = True)
    fas = Graph.feedback_arc_set(net)
    return fas
