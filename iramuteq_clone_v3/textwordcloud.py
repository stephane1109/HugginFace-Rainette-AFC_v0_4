# -*- coding: utf-8 -*-
#Author: Pierre Ratinaud
#Copyright (c) 2008-2020 Pierre Ratinaud
#modification pour python 3 : Laurent Mérat, 6x7 - mai 2020
#License: GNU/GPL

#------------------------------------
# import des modules python
#------------------------------------
import tempfile
import os
import logging

#------------------------------------
# import des modules wx
#------------------------------------
import wx

#------------------------------------
# import des fichiers du projet
#------------------------------------
from analysetxt import AnalyseText
from guifunct import getPage, getCorpus, SelectColumn
from functions import sortedby, progressbar 
from dialog import StatDialog, PrefWordCloud
from PrintRScript import WordCloudRScript


logger = logging.getLogger('iramuteq.textwordcloud')


class WordCloud(AnalyseText):

    def doanalyse(self) :
        self.parametres['type'] = 'wordcloud'
        #FIXME
        limit = 3

#        self.dlg.Destroy()

        res = self.make_option()
        if res == wx.ID_OK :
            if self.parametres['mode'] == 2 :
                self.actives = self.corpus.make_actives_limit(limit, 1)
                self.actives += self.corpus.make_actives_limit(limit, 2)
            elif self.parametres['mode'] == 0 :
                self.actives = self.corpus.make_actives_limit(limit, 1)
            elif self.parametres['mode'] == 1 :
                self.actives = self.corpus.make_actives_limit(limit, 2)
            dictcol = dict([[i, [act, self.corpus.getlemeff(act)]] for i, act in enumerate(self.actives)]) 
            selectcol = SelectColumn(self.ira, dictcol, self.actives, self.pathout['selected.csv'], dlg = True)
            if selectcol.ok :
                self.dlg = progressbar(self.ira, 2)
                self.make_wordcloud()
                script = WordCloudRScript(self)
                script.make_script()
                self.doR(script.scriptout, dlg = self.dlg, message = 'R...')
                self.dlg.Destroy()
            else :
                return 'NOK'
        else :
            return 'NOK'

    def make_option(self, fromcluster = False) :
        dial = PrefWordCloud(self.ira, fromcluster)
        dial.CenterOnParent()
        res = dial.ShowModal()
        if res == wx.ID_OK :
            if dial.format.GetSelection() == 0 :
                svg = 0
            else :
                svg = 1
            self.parametres['width'] = dial.spin_L.GetValue()
            self.parametres['height'] = dial.spin_H.GetValue()
            self.parametres['maxword'] = dial.spin_maxword.GetValue()
            self.parametres['mincex'] = float(dial.spin_mincex.GetValue())/float(10)
            self.parametres['maxcex'] = float(dial.spin_maxcex.GetValue())/float(10)
            self.parametres['col_text'] = dial.color_text.GetColour()
            self.parametres['col_bg'] = dial.color_bg.GetColour()
            self.parametres['mode'] = dial.typeformeschoice.GetSelection()
            self.parametres['svg'] = svg
            if fromcluster :
                self.parametres['indice'] = dial.indice.GetSelection()
            outgraph = os.path.join(os.path.dirname(self.pathout['zipf.png']), 'nuage_')
            nb = 1
            if svg :
                end = '.svg'
            else :
                end = '.png'
            while os.path.exists(outgraph + str(nb) + end) :
                nb += 1
            self.parametres['graphout'] = outgraph + str(nb) + end
        dial.Destroy()
        return res

    def make_wordcloud(self) :
        act = ['\t'.join([act, repr(self.corpus.getlemeff(act))]) for act in self.actives]
        with open(self.pathout['actives_eff.csv'], 'w', encoding='utf8') as f :
            f.write('\n'.join(act))


class ClusterCloud(WordCloud):

    def doanalyse(self):
        print('ClusterCloud')
        self.parametres['type'] = 'clustercloud'
        #FIXME
        limit = 2
        res = self.make_option(True) #dialogue d'options de WordCloud.make_option
        if res == wx.ID_OK :
            prof = self.parametres['clusterprof']
            del self.parametres['clusterprof']
            if self.parametres['indice'] == 0:
                tokeep = 1
            else: 
                tokeep = 2
            prof = [[val[0], int(round(val[tokeep]))] for val in prof]
            with open(self.pathout['actives_eff.csv'], 'w', encoding='utf8') as f :
                f.write('\n'.join(['\t'.join([val[0], repr(val[1])]) for val in prof]))
            dictcol = dict([[i, val] for i, val in enumerate(prof)])
            self.actives = [val[0] for val in prof]
            SelectColumn(self.ira, dictcol, self.actives, self.pathout['selected.csv'], dlg = True)
            script = WordCloudRScript(self)
            script.make_script()

            dialProgression = progressbar(self.ira, self.dlg)
            self.doR(script.scriptout, dlg = dialProgression, message = 'R...')
            dialProgression.Destroy()

        else:
            return 'NOK'

