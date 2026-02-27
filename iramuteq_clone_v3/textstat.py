# -*- coding: utf-8 -*-
#Author: Pierre Ratinaud
#Copyright (c) 2008-2020 Pierre Ratinaud
#modification pour python 3 : Laurent Mérat, 6x7 - mai 2020
#License: GNU/GPL

#------------------------------------
# import des modules python
#------------------------------------
import tempfile
from time import sleep
import logging

import langue
langue.run()

#------------------------------------
# import des fichiers du projet
#------------------------------------
from chemins import ffr
from analysetxt import AnalyseText
from functions import sortedby, progressbar, exec_rcode, check_Rresult


logger = logging.getLogger('iramuteq.textstat')


class Stat(AnalyseText) :

    def doanalyse(self) :
        self.make_stats()

    def preferences(self) :
        return self.parametres

    def make_stats(self):
#        if self.dlg :
#        if not 'dlg' in dir(self) :
        self.dlg = progressbar(self, 7)
        formes = self.corpus.lems
        tot = [[forme, formes[forme].freq, formes[forme].gram] for forme in formes if formes[forme].freq > 1]
        tot = sortedby(tot, 2, 1)
        tot = [[i, val] for i, val in enumerate(tot)]
        hapax = [[forme, formes[forme].freq, formes[forme].gram] for forme in formes if formes[forme].freq == 1]
        hapax = sortedby(hapax, 1, 1)
        hapax = [[i, val] for i, val in enumerate(hapax)]
        act = [[forme, formes[forme].freq, formes[forme].gram] for forme in formes if formes[forme].act == 1]
        act = sortedby(act, 2, 1)
        act = [[i, val] for i, val in enumerate(act)]
        supp = [[forme, formes[forme].freq, formes[forme].gram] for forme in formes if formes[forme].act == 2]
        supp = sortedby(supp, 2, 1)
        supp = [[i, val] for i, val in enumerate(supp)]
        ucesize = self.corpus.getucesize()
        with open(self.pathout['stsize.csv'], 'w', encoding='utf8') as f :
            f.write('\n'.join([repr(val) for val in ucesize]))
        self.result = {'total' : dict(tot), 'formes_actives' : dict(act), 'formes_supplémentaires' : dict(supp), 'hapax' : dict(hapax), 'glob' : ''}
        occurrences = sum([val[1][1] for val in tot]) + len(hapax)
        phapax = (float(len(hapax)) / float(occurrences)) * 100
        phapax_forme = (float(len(hapax)) / (float(len(formes)))) * 100
        moy_occu_mot = float(occurrences) / float(len(formes))
        txt = ''.join([_('Abstract'), '\n'])
        txt += ''.join([_('Number of texts'),' : ', '%i\n' % len(self.corpus.ucis)])
        txt += ''.join([_("Number of occurrences"),' : %i\n' % occurrences])
        txt += ''.join([_('Number of forms'), ' : %i\n' % (len(formes))])
        txt += ''.join([_("Number of hapax"),' : %i (%.2f%% ' % (len(hapax),phapax), _('of occurrences'), ' - %.2f%% ' % phapax_forme, _('of forms'), ')\n'])
        txt += ''.join([_("Mean of occurrences by text"), ' : %.2f' % (float(occurrences)/float(len(self.corpus.ucis)))])
        if self.dlg :
            self.dlg.Update(7, 'Ecriture...')
        self.result['glob'] = txt
        self.print_result()
        # for Zipf grap
        txt = """
        source("%s")
        tot <- read.csv2("%s", header = FALSE, row.names = 1)
        """ % (ffr(self.parent.RscriptsPath['Rgraph']), ffr(self.pathout['total.csv']))
        if len(hapax) :
            txt += """
            hapax <- read.csv2("%s", header = FALSE, row.names = 1)
            tot <- rbind(tot, hapax)
            """ % ffr(self.pathout['hapax.csv'])
        txt += """
        open_file_graph("%s", width = 400, height = 400)
        plot(tot[,1], log = 'xy', xlab='log(rangs)', ylab = 'log(frequences)', col = 'red', pch=16)
        dev.off()
        """ % (ffr(self.pathout['zipf.png']))
        txt += """
        stsize <- read.csv2("%s", header=F)
        open_file_graph("%s", width = 400, height = 400)
        barplot(table(stsize[,1]))
        dev.off()
        """ % (ffr(self.pathout['stsize.csv']), ffr(self.pathout['segments_size.png']))
        tmpscript = tempfile.mktemp(dir=self.parent.TEMPDIR)
        with open(tmpscript, 'w', encoding='utf8') as f :
            f.write(txt)
        pid = exec_rcode(self.parent.RPath, tmpscript, wait = False)
        while pid.poll() == None :
            sleep(0.2)
        check_Rresult(self.parent, pid)
        if self.dlg :
            self.dlg.Destroy()

    def print_result(self) :
        for key in self.result :
            if key != 'glob' :
                dico = self.result[key]
                toprint = [[dico[val][0],repr(dico[val][1]), dico[val][2]] for val in dico]
                with open(self.pathout['%s.csv' % key], 'w',  encoding='utf8') as f :
                    f.write('\n'.join([';'.join([val for val in ligne]) for ligne in toprint]))
            else :
                with open(self.pathout['%s.txt' % 'glob'], 'w',  encoding='utf8') as f :
                    f.write(self.result['glob'])
