# -*- coding: utf-8 -*-
#Author: Pierre Ratinaud
#Copyright (c) 2008-2020 Pierre Ratinaud
#modification pour python 3 : Laurent Mérat, 6x7 - mai 2020
#License: GNU/GPL

#------------------------------------
# import des modules python
#------------------------------------
import os


class ClasseCHD :

    def __init__(self, parent, corpus, classe, cmd = False) :
        self.parent = parent
        self.cmd = cmd
        self.corpus = self.corpus_from_classe(corpus, classe, False)

    def corpus_from_classe(self, corpus, classe, lem) :
        if lem :
            ucis_paras_uces = corpus.make_ucis_paras_uces_lems()
        else :
            ucis_paras_uces = corpus.ucis_paras_uces
        ucecl = {}
        for i, lc in enumerate(corpus.lc) :
            for uce in lc :
                ucecl[uce] = i + 1
        for uce in corpus.lc0 :
            ucecl[uce] = 0
        ucecltri = list(ucecl.keys())
        #ucecltri = [[int(val) for val in uce] for uce in ucecltri]
        ucecltri.sort()
        res = [['**** *classe_%i ' % ucecl[uce] + ' '.join(corpus.etoiles[uce[0]][uce[1]][uce[2]]), ' '.join(ucis_paras_uces[uce[0]][uce[1]][uce[2]])] for uce in ucecltri if ucecl[uce] == classe]
        fileout = os.path.dirname(corpus.dictpathout['ira'])
        fileout = os.path.join(fileout, 'corpus_classe_%i.txt' % classe)
        print(fileout)
        with open(fileout,'w') as f :
            f.write('\n'.join(['\n'.join(uce) for uce in res]))
        self.parent.filename = fileout
        if not self.cmd :
            self.parent.OpenText()
