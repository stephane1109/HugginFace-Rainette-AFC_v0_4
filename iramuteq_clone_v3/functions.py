# -*- coding: utf-8 -*-
#Author: Pierre Ratinaud
#Copyright (c) 2008-2020 Pierre Ratinaud
#modification pour python 3 : Laurent Mérat, 6x7 - mai 2020
#License: GNU/GPL

#------------------------------------
# import des modules python
#------------------------------------
import re
from subprocess import Popen, call, PIPE
import _thread
import os
import ast
import sys
import csv
import platform
import traceback
import codecs
import locale
import datetime
from copy import copy
from shutil import copyfile
import shelve
import json
#from dialog import BugDialog
import logging
from operator import itemgetter
import ssl
ssl._create_default_https_context = ssl._create_unverified_context

#------------------------------------
# import des modules wx
#------------------------------------
import wx
import wx.adv

#------------------------------------
# import des fichiers du projet
#------------------------------------
from configparser import ConfigParser


log = logging.getLogger('iramuteq')


indices_simi = ['cooccurrence' ,'pourcentage de cooccurrence','Russel','Jaccard', 'Kulczynski1', 'Kulczynski2', 'Mountford', 'Fager', 'simple matching', 'Hamman', 'Faith', 'Tanimoto', 'Dice', 'Phi', 'Stiles', 'Michael', 'Mozley', 'Yule', 'Yule2', 'Ochiai', 'Simpson', 'Braun-Blanquet','Chi-squared', 'Phi-squared', 'Tschuprow', 'Cramer', 'Pearson', 'binomial']

def open_folder(folder):
    if sys.platform == "win32":
        os.startfile(folder)
    else:
        opener ="open" if sys.platform == "darwin" else "xdg-open"
        #call([opener, folder])
        call(["%s '%s' &" % (opener, folder)], shell=True)

def normpath_win32(path) :
    if not sys.platform == 'win32' :
        return path
    while '\\\\' in path :
        path = path.replace('\\\\', '\\')
    if path.startswith('\\') and not path.startswith('\\\\') :
        path = '\\' + path
    return path

class TGen :
    def __init__(self, path = None, encoding = 'utf8'):
        self.path = path
        self.tgen = {}
        self.encoding = encoding

    def __getitem__(self, key):
        return self.tgen[key]

    def read(self, path = None):
        if path is None :
            path = self.path
        with codecs.open(path, 'r', self.encoding) as f :
            tgen = f.read()
        tgen = [line.split('\t') for line in tgen.splitlines()]
        tgen = dict([[line[0], line[1:]] for line in tgen])
        self.tgen = tgen
        self.path = path

    def write(self, path = None):
        if path is None :
            path = self.path
        with open(path, 'w', encoding='utf8') as f :
            f.write('\n'.join(['\t'.join([val] + self.tgen[val]) for val in self.tgen]))

    def writetable(self, pathout, tgens, totocc):
        etoiles = list(totocc.keys())
        etoiles.sort()
        with open(pathout, 'w', encoding='utf8') as f :
            line = '\t'.join(['tgens'] + etoiles) + '\n'
            f.write(line)
            for t in tgens :
                line = '\t'.join([t] + [repr(tgens[t][et]) for et in etoiles]) + '\n'
                f.write(line)
            i = 0
            totname = 'total'
            while totname + repr(i) in tgens :
                i += 1
            totname = totname + repr(i)
            line = '\t'.join([totname] + [repr(totocc[et]) for et in etoiles]) + '\n'
            f.write(line)

class History :
    def __init__(self, filein, syscoding = 'utf8') :
        self.filein = filein
        self.syscoding = syscoding
        self.corpus = {}
        self.openedcorpus = {}
        self.openedmatrix = {}
        self.orph = []
        self.analyses = {}
        self.history = []
        self.opened = {}
        self.read()

    def read(self) :
        with open(self.filein, 'r', encoding='utf8') as fjson :
            d = json.load(fjson)
#        d = shelve.open(self.filein, protocol=1)
        self.history = d.get('history', [])
        self.matrix = d.get('matrix', [])
        self.ordercorpus = dict([[corpus['uuid'], i] for i, corpus in enumerate(self.history)])
        self.corpus = dict([[corpus['uuid'], corpus] for corpus in self.history])
        self.analyses = dict([[analyse['uuid'], analyse] for corpus in self.history for analyse in corpus.get('analyses', [])])
        self.matrixanalyse = dict([[mat['uuid'], mat] for mat in self.matrix])
        self.ordermatrix = dict([[matrix['uuid'], i] for i, matrix in enumerate(self.matrix)])
#        d.close()

    def write(self) :
        d = {}
        d['history'] = self.history
        d['matrix'] = self.matrix
        with open(self.filein, 'w', encoding='utf8') as f :
            f.write(json.dumps(d, indent=4, default=str))
       #d = shelve.open(self.filein, protocol=1)
       #d.close()

    def add(self, analyse) :
        log.info('add to history %s' % analyse.get('corpus_name', 'pas un corpus'))
        tosave = {'uuid' : analyse['uuid'], 'ira': analyse['ira'], 'type' : analyse['type']}
        if tosave['uuid'] in self.corpus :
            log.info('problem : this uuid is already in history : %s' % tosave['uuid'])
            return
        if analyse.get('corpus', False) :
            if analyse['uuid'] in self.analyses :
                return
            tosave['corpus'] = analyse['corpus']
            tosave['name'] = analyse['name']
            acorpus_uuid =  analyse['corpus']
            if acorpus_uuid in self.corpus :
                if 'analyses' in self.history[self.ordercorpus[acorpus_uuid]] :
                    self.history[self.ordercorpus[acorpus_uuid]]['analyses'].append(tosave)
                else :
                    self.history[self.ordercorpus[acorpus_uuid]]['analyses'] = [tosave]
            else :
                self.orph.append(tosave)
        else :
            tosave['corpus_name'] = analyse['corpus_name']
            #self.ordercorpus[tosave['uuid']] = len(history)
            #self.corpus[tosave['uuid']] = analyse
            self.history.append(tosave)
        self.write()
        self.read()

    def addMatrix(self, analyse) :
        tosave = analyse
        #tosave['matrix_name'] = analyse['matrix_name']
        tosave['analyses'] = []
        self.matrix.append(tosave)
        self.write()
        self.read()

    def addMatrixAnalyse(self, analyse) :
        tosave = {'uuid' : analyse['uuid'], 'ira': analyse['ira'], 'type' : analyse['type'], 'matrix' : analyse['matrix']}
        tosave['name'] = analyse['name']
        if tosave['matrix'] in self.ordermatrix :
            self.matrix[self.ordermatrix[tosave['matrix']]]['analyses'].append(tosave)
        self.write()
        self.read()

    def addmultiple(self, analyses) :
        log.info('add multiple')
        for analyse in analyses :
            tosave = {'uuid' : analyse['uuid'], 'ira': analyse['ira'], 'type' : analyse['type']}
            corpus = analyse['corpus']
            tosave['corpus'] = corpus
            tosave['name'] = analyse['name']
            if corpus in self.corpus :
                if 'analyses' in self.history[self.ordercorpus[corpus]] :
                    self.history[self.ordercorpus[corpus]]['analyses'].append(tosave)
                else :
                    self.history[self.ordercorpus[corpus]]['analyses'] = [tosave]
        self.write()
        self.read()

    def delete(self, analyse, corpus = False) :
        log.info('delete %s' % analyse.get('name', 'noname'))
        if corpus :
            self.history.pop(self.ordercorpus[analyse['uuid']])
            if analyse['uuid'] in self.openedcorpus :
                del self.openedcorpus[analyse['uuid']]
            log.info('delete corpus : %s' % analyse['uuid'])
        elif analyse['uuid'] in self.analyses :
            todel = [i for i, ana in enumerate(self.corpus[analyse['corpus']]['analyses']) if ana['uuid'] == analyse['uuid']][0]
            self.history[self.ordercorpus[analyse['corpus']]]['analyses'].pop(todel)
        elif analyse['uuid'] in self.matrixanalyse :
            self.matrix = [mat for mat in self.matrix if mat['uuid'] != analyse['uuid']]
        elif analyse.get('matrix', False) in self.matrixanalyse :
            analyses = self.matrix[self.ordermatrix[analyse['matrix']]]['analyses']
            topop = [i for i, val in enumerate(analyses) if analyse['uuid'] == val['uuid']][0]
            analyses.pop(topop)
            self.matrix[self.ordermatrix[analyse['matrix']]]['analyses'] = analyses
        self.write()
        self.read()

    def addtab(self, analyse) :
        self.opened[analyse['uuid']] = analyse

    def rmtab(self, analyse) :
        del self.opened[analyse['uuid']]

    def update(self, analyse) :
        if 'matrix_name' in analyse :
            self.matrixanalyse[analyse['uuid']].update(analyse)
        elif 'corpus_name' in analyse :
            self.corpus[analyse['uuid']].update(analyse)
        elif 'corpus' in analyse :
            self.analyses[analyse['uuid']].update(analyse)
        else :
            toupdate = [an for an in self.matrixanalyse[analyse['matrix']]['analyses'] if an['uuid'] == analyse['uuid']]
            toupdate[0].update(analyse)
        self.write()
        self.read()

    def clean(self) :
        corpustodel = [corpus for corpus in self.history if not os.path.exists(corpus['ira'])]
        print(corpustodel)
        for corpus in corpustodel :
            print('cleaning :', corpus['corpus_name'])
            self.delete(corpus, corpus = True)
        anatodel = [analyse for corpus in self.history for analyse in corpus.get('analyses', []) if not os.path.exists(analyse.get('ira', '/'))]
        for analyse in anatodel :
            print('cleaning :', analyse['name'])
            self.delete(analyse)

    def dostat(self):
        todel = {}
        tokens = 0
        corpusnb = {}
        subnb = 0
        analysenb = 0
        hours = 0
        minutes = 0
        secondes = 0
        ha = 0
        ma = 0
        sa = 0
        for corpus in self.history :
            analysenb += len(corpus.get('analyses', []))
            analyses = corpus.get('analyses', [])
            for analyse in analyses :
                if os.path.exists(analyse['ira']) :
                    ana = DoConf(analyse['ira']).getoptions()
                    if 'time' in ana :
                        time = ana['time'].split()
                        ha += int(time[0].replace('h','')) * 3600
                        ma += int(time[1].replace('m','')) * 60
                        sa += int(time[2].replace('s',''))
            if os.path.exists(corpus['ira']) :
                param = DoConf(corpus['ira']).getoptions()
                time = param.get('time','0h 0m 0s')
                time = time.split()
                hours += int(time[0].replace('h','')) * 3600
                minutes += int(time[1].replace('m','')) * 60
                secondes += int(time[2].replace('s',''))
                if param.get('originalpath', False) :
                    if param['originalpath'] in corpusnb :
                        corpusnb[param['originalpath']] += 1
                        tokens += int(param['occurrences'])
                    else :
                        corpusnb[param['originalpath']] = 1
                    #print param
                else :
                    subnb += 1
            else :
                if corpus['ira'] in todel :
                    todel['ira'] += 1
                else :
                    todel['ira'] = 1
        print('Nbr total de corpus : %s' % len(self.history))
        corpus_nb = len(corpusnb) + len(todel)
        print('Nbr de corpus différents : %s' % corpus_nb)
        lentodel = len(todel)
        print('Nbr de corpus à supprimer : %s' % lentodel)
        print('Nbr de sous corpus : %s' % subnb)
        print("Nbr total d'occurrences : %s" % tokens)
        print('Moyenne occurrences par corpus : %f' % (tokens/corpus_nb))
        print('---------------------')
        print("Nbr total d'analyses : %s" % analysenb)
        print('Temps total indexation : %f h' % ((hours+minutes+secondes) / 3600))
        print('Temps total analyses :  %f h' % ((ha+ma+sa) / 3600))

    def __str__(self) :
        return str(self.history)

class DoConf :
    def __init__(self, configfile=None, diff = None, parametres = None) :
        self.configfile = configfile
        self.conf = ConfigParser(interpolation=None) # pourquoi ce paramètre ???

        if configfile is not None :
            configfile = normpath_win32(configfile)
            self.conf.read_file(codecs.open(configfile, 'r', 'utf8'))
        self.parametres = {}
        if parametres is not None :
            self.doparametres(parametres)

    def doparametres(self, parametres) :
        return parametres

    def getsections(self) :
        return self.conf.sections()

    def getoptions(self, section = None, diff = None):
        parametres = {}
        if section is None :
            section = self.conf.sections()[0]
        for option in self.conf.options(section) :
            if self.conf.get(section, option).isdigit() :
                parametres[option] = int(self.conf.get(section, option))
            elif self.conf.get(section, option) == 'False' :
                parametres[option] = False
            elif self.conf.get(section, option) == 'True' :
                parametres[option] = True
            elif self.conf.get(section, option).startswith('(') and self.conf.get(section, option).endswith(')') :
                parametres[option] = ast.literal_eval(self.conf.get(section, option))
            elif self.conf.get(section, option).startswith('[') and self.conf.get(section, option).endswith(']') :
                parametres[option] = ast.literal_eval(self.conf.get(section, option))
            else :
                parametres[option] = self.conf.get(section, option)
        if 'type' not in parametres :
            parametres['type'] = section
        return parametres

    def makeoptions(self, sections, parametres, outfile = None) :
        txt = ''
        for i, section in enumerate(sections) :
            txt += '[%s]\n' % section
            if not self.conf.has_section(section) :
                self.conf.add_section(section)
            for option in parametres[i] :
                if isinstance(parametres[i][option], int) :
                    self.conf.set(section, option, repr(parametres[i][option]))
                    txt += '%s = %i\n' % (option, parametres[i][option])
                elif isinstance(parametres[i][option], str) :
                    self.conf.set(section, option, parametres[i][option])
                    txt += '%s = %s\n' % (option, parametres[i][option])
                elif isinstance(parametres[i][option], wx.Colour) :
                    self.conf.set(section, option, str(parametres[i][option]))
                    txt += '%s = %s\n' % (option, str(parametres[i][option]))
                elif option == 'analyses' :
                    pass
                else :
                    self.conf.set(section, option, repr(parametres[i][option]))
                    txt += '%s = %s\n' % (option, repr(parametres[i][option]))
        if outfile is None :
            outfile = self.configfile
        outfile = normpath_win32(outfile)
        with open(outfile, 'w', encoding="utf-8") as f :
            f.write(txt)
            #self.conf.write(f)

    def totext(self, parametres) :
        #txt = ['Corpus']
        txt = []
        for val in parametres :
            if isinstance(parametres[val], int) :
                txt.append(' \t\t: '.join([val, repr(parametres[val])]))
            elif isinstance(parametres[val], str) :
                txt.append(' \t\t: '.join([val, parametres[val]]))
            elif val in ['listet', 'stars'] :
                pass
            else :
                txt.append(' \t\t: '.join([val, repr(parametres[val])]))
        return '\n'.join(txt)


def write_tab(tab, fileout) :
        csvWriter = csv.writer(open(fileout, 'w', newline='', encoding='utf8'), delimiter=';', quoting = csv.QUOTE_NONNUMERIC)
        csvWriter.writerows(tab)

class BugDialog(wx.Dialog):
    def __init__(self, *args, **kwds):
        # begin wxGlade: MyDialog.__init__
        kwds["style"] = wx.DEFAULT_DIALOG_STYLE | wx.STAY_ON_TOP
        kwds["size"] = wx.Size(500, 200)
        wx.Dialog.__init__(self, *args, **kwds)
        self.SetTitle(kwds['title'])
        self.text_ctrl_1 = wx.TextCtrl(self, -1, "", style=wx.TE_MULTILINE)
        self.text_ctrl_1.SetBackgroundColour('#DDE8EB')
        self.button_1 = wx.Button(self, wx.ID_OK, "")

        self.__set_properties()
        self.__do_layout()
        # end wxGlade

    def __set_properties(self):
        # begin wxGlade: MyDialog.__set_properties
        self.SetMinSize(wx.Size(500, 200))
        self.text_ctrl_1.SetMinSize(wx.Size(500, 200))

        # end wxGlade

    def __do_layout(self):
        # begin wxGlade: MyDialog.__do_layout
        sizer_1 = wx.BoxSizer(wx.VERTICAL)
        sizer_1.Add(self.text_ctrl_1, 1, wx.EXPAND, 0)
        sizer_1.Add(self.button_1, 0, wx.ALIGN_CENTER_HORIZONTAL, 0)
        self.SetSizer(sizer_1)
        sizer_1.Fit(self)
        self.Layout()


def CreateIraFile(DictPathOut, clusternb, corpname='corpus_name', section = 'analyse'):
    AnalyseConf = ConfigParser()
    AnalyseConf.read(DictPathOut['ira'])
    AnalyseConf.add_section(section)
    date = datetime.datetime.now().ctime()
    AnalyseConf.set(section, 'date', str(date))
    AnalyseConf.set(section, 'clusternb', clusternb)
    AnalyseConf.set(section, 'corpus_name', corpname)

    fileout = open(DictPathOut['ira'], 'w', encoding='utf8')
    AnalyseConf.write(fileout)
    fileout.close()

def multisort(liste2d, ordre, indices_tri):

    """
    methode destinée à remplacer 'comp' qui a disparu en Python 3
        tri de tuples sur l'un des éléments du tuple
        en principe, elle doit renvoyer les éléments triés selon le principe d'avant
        tel que décrit dans la docstring de 'sortedby'

        probablement à améliorer pour la rendre d'usage plus général
        en acceptant un nombre variable de parametres ???
    """

    indices_triTuple = indices_tri.Tuple(int, ...)
    for key in reversed(indices_tri):
        liste2d.sort(key=attrgetter(key), reverse=ordre)
    return liste2d

def sortedby(liste2d, direct, *indices):

    """
        sortedby: sort a list of lists (e.g. a table) by one or more indices
                  (columns of the table) and return the sorted list

        e.g.
         for list = [[2,3],[1,2],[3,1]]:
         sortedby(list,1) will return [[3, 1], [1, 2], [2, 3]],
         sortedby(list,0) will return [[1, 2], [2, 3], [3, 1]]

         elle n'est pas remplacée par la méthode 'multisort' ???

    """

# iramuteq original
#    nlist = map(lambda x, indices=indices:
#                 map(lambda i, x=x: x[i], indices) + [x],
#                 list)

# iramuteq passé à 2to3
#    nlist = list(map(lambda x, indices=indices:
#                 list(map(lambda i, x=x: x[i], indices)) + [x],
#                 liste2d))

    for key in reversed(indices):
        liste2d.sort(key=itemgetter(key), reverse=(direct==2))
    return liste2d


#    if direct == 1:
#        nlist.sort()
#         sorted_list = multisort(liste2d, direct, *indices)

#    elif direct == 2:
#        nlist.sort(reverse=True)
#         sorted_list = multisort(liste2d, direct, *indices)

#    return [l[-1] for l in nlist]
#    return sorted_list

def add_type(line, dictlem):
    if line[4] in dictlem:
        line.append(dictlem[line[4]])
    else :
        line.append('')
    return line

def treat_line_alceste(i, line) :
    if line[0] == '*' or line[0] == '*****' :
        return line + ['']
    if line[5] == 'NA':
        print('NA', line[5])
        pass
    elif float(line[5].replace(',', '.')) < 0.0001:
        line[5] = '< 0,0001'
    elif float(line[5].replace(',', '.')) > 0.05:
        line[5] = 'NS (%s)' % str(float(line[5].replace(',', '.')))[0:7]
    else:
        line[5] = str(float(line[5].replace(',', '.')))[0:7]
    return [i, int(line[0]), int(line[1]), float(line[2]), float(line[3]), line[6], line[4], line[5]]

def ReadProfileAsDico(File, Alceste=False, encoding = 'utf8'):
    dictlem = {}
    print('lecture des profiles')
    FileReader = open(File, 'r', encoding='utf8')
    Filecontent = FileReader.readlines()
    FileReader.close()
    DictProfile = {}
    count = 0
    #rows = [row.replace('\n', '').replace("'", '').replace('\"', '').replace(',', '.').replace('\r','').split(';') for row in Filecontent]
    rows = [row.replace('\n', '').replace("'", '').replace('\"', '').replace('\r','').split(';') for row in Filecontent]
    rows.pop(0)
    ClusterNb = rows[0][2]
    rows.pop(0)
    clusters = [row[2] for row in rows if row[0] == '**']
    valclusters = [row[1:4] for row in rows if row[0] == '****']
    lp = [i for i, line in enumerate(rows) if line[0] == '****']
    prof = [rows[lp[i] + 1:lp[i+1] - 1] for i in range(0, len(lp)-1)] + [rows[lp[-1] + 1:len(rows)]]
    if Alceste :
        prof = [[add_type(row, dictlem) for row in pr] for pr in prof]
        prof = [[treat_line_alceste(i,line) for i, line in enumerate(pr)] for pr in prof]
    else :
        prof = [[line + [''] for line in pr] for pr in prof]
        prof = [[treat_line_alceste(i,line) for i, line in enumerate(pr)] for pr in prof]
    for i, cluster in enumerate(clusters):
        DictProfile[cluster] = [valclusters[i]] + prof[i]
    return DictProfile

def GetTxtProfile(dictprofile, cluster_size) :
    proflist = []
    for classe in range(0, len(dictprofile)) :
        prof = dictprofile[str(classe + 1)]
        clinfo = cluster_size[classe]
        proflist.append('\n'.join([' '.join(['classe %i' % (classe + 1), '-', '%s uce sur %s - %s%%' % (clinfo[0], clinfo[1], clinfo[2])]), '\n'.join(['%5s|%5s|%6s|%6s|%8s|%8s|%20s\t%10s' % tuple([str(val) for val in line]) for line in prof if len(line)==8])]))
    return '\n\n'.join(proflist)

def formatExceptionInfo(maxTBlevel=5):
    cla, exc, trbk = sys.exc_info()
    try :
        excName = cla.__name__
    except :
        excName = 'None'
    try:
        excArgs = exc.args[0]
    except :
        excArgs = "<no args>"
    excTb = traceback.format_tb(trbk, maxTBlevel)
    return (excName, excArgs, excTb)


#fonction des etudiants de l'iut
def decoupercharact(chaine, longueur, longueurOptimale, separateurs = None) :
    """
        on part du dernier caractère, et on recule jusqu'au début de la chaîne.
        Si on trouve un '$', c'est fini.
        Sinon, on cherche le meilleur candidat. C'est-à-dire le rapport poids/distance le plus important.
    """
    separateurs = [['.', 60.0], ['?', 60.0], ['!', 60.0], ['£$£', 60], [':', 50.0], [';', 40.0], [',', 10.0], [' ', 0.1]]
    trouve = False                 # si on a trouvé un bon séparateur
    iDecoupe = 0                # indice du caractere ou il faut decouper

    # on découpe la chaine pour avoir au maximum 240 caractères
    longueur = min(longueur, len(chaine) - 1)
    chaineTravail = chaine[:longueur + 1]
    nbCar = longueur
    meilleur = ['', 0, 0]        # type, poids et position du meilleur separateur

    # on vérifie si on ne trouve pas un '$'
    indice = chaineTravail.find('$')
    if indice > -1:
        trouve = True
        iDecoupe = indice

    # si on ne trouve rien, on cherche le meilleur séparateur
    if not trouve:
        while nbCar >= 0:
            caractere = chaineTravail[nbCar]
            distance = abs(longueurOptimale - nbCar) + 1
            meilleureDistance = abs(longueurOptimale - meilleur[2]) + 1

            # on vérifie si le caractére courant est une marque de ponctuation
            for s in separateurs:
                if caractere == s[0]:
                    # si c'est une ponctuation

                    if s[1] / distance > float(meilleur[1]) / meilleureDistance:
                        # print nbCar, s[0]
                        meilleur[0] = s[0]
                        meilleur[1] = s[1]
                        meilleur[2] = nbCar
                        trouve = True
                        iDecoupe = nbCar

                    # et on termine la recherche
                    break

            # on passe au caractère précédant
            nbCar = nbCar - 1

    # si on a trouvé
    if trouve:
        fin = chaine[iDecoupe + 1:]
        retour = chaineTravail[:iDecoupe]
        return len(retour) > 0, retour.split(), fin
    # si on a rien trouvé
    return False, chaine.split(), ''


exceptions = {'paragrapheOT' : "Un problème de formatage (présence d'un marqueur de paragraphe (-*) en dehors d'un texte) est survenu à la ligne ",
              'EmptyText' : "Texte vide (probablement un problème de formatage du corpus). Le problème est apparu à la ligne ",
              'CorpusEncoding' : "Problème d'encodage.",
              'TextBeforeTextMark' : "Problème de formatage : du texte avant le premier marqueur de texte (****). Le problème est survenu à la ligne ",
              'MissingAnalyse' : 'Aucun fichier à cet emplacement :\n',
}

def BugReport(parent, error = None):
    for ch in parent.GetChildren():
        if "<class 'wx._windows.ProgressDialog'>" == str(type(ch)):
            ch.Destroy()
    excName, exc, excTb = formatExceptionInfo()
    if excName == 'Exception' :
        print(exc)
        if len(exc.split()) == 2 :
            mss, linenb = exc.split()
            if mss in exceptions :
                txt = exceptions[mss] + linenb
            else :
                txt = exc
        else :
            if exc in exceptions :
                txt = exceptions[exc]
            else :
                txt = exc
        title = "Information"
    else :
        txt = '\n            !== BUG ==!       \n'
        txt += '*************************************\n'
        txt += '\n'.join(excTb).replace('    ', ' ')
        txt += excName + '\n'
        txt += repr(exc)
        title = "Bug"

    dial = BugDialog(parent, **{'title' : title})
    if 'Rerror' in dir(parent) :
        txt += parent.Rerror
        parent.Rerror = ''
    log.info(txt)
    dial.text_ctrl_1.write(txt)
    dial.CenterOnParent()
    dial.ShowModal()
    dial.Destroy()

def PlaySound(parent):
    if parent.pref.getboolean('iramuteq', 'sound') :
        try:
            if "gtk2" in wx.PlatformInfo:
                error = Popen(['aplay','-q',os.path.join(parent.AppliPath,'son_fin.wav')])
            else :
                sound = wx.adv.Sound(os.path.join(parent.AppliPath, 'son_fin.wav'))
                sound.Play(wx.adv.SOUND_SYNC)
        except :
            print('pas de son')

def ReadDicoAsDico(dicopath):
    with open(dicopath, 'r', encoding='UTF8') as f:
        content = f.readlines()
    lines = [line.rstrip('\n\r').replace('\n', '').replace('"', '').split('\t') for line in content if line != '']
    return dict([[line[0], line[1:]] for line in lines])

def ReadLexique(parent, lang = 'french', filein = None):
    if lang != 'other' :
        if filein is None :
            parent.lexique = ReadDicoAsDico(parent.DictPath.get(lang, 'french'))
        else :
            parent.lexique = ReadDicoAsDico(filein)
    else :
        if filein is None :
            parent.lexique = {}
        else :
            parent.lexique = ReadDicoAsDico(filein)

def ReadList(filein, encoding = 'utf8', sep = ';'):
    with open(filein, 'r', encoding='utf8') as f :
        content = f.read()
    content = [line.replace('\n', '').replace('\r','').replace('\"', '').replace(',', '.').split(sep) for line in content.splitlines()]
    first = content.pop(0)
    dico = {}
    i = 0
    for line in content:
        nline = [line[0]]
        for val in line[1:]:
            if val == 'NA' :
                don = ''
            else:
                try:
                    don = int(val)
                except:
                    don = float('%.5f' % float(val))
            nline.append(don)
        dico[i] = nline
        i += 1
    return dico, first

def readliststat(filein, sep='\t') :
    with open(filein, 'r', encoding='utf8') as f :
        content = f.read()
    content = [line.replace('\n', '').replace('\r','').replace('\"', '').replace(',', '.').split(sep) for line in content.splitlines()]
    dico = {}
    content = list(map(list, zip(*content)))
    first = content.pop(0)
    i=0
    for line in content:
        nline = [line[0]]
        for val in line[1:]:
            if val == 'NA' :
                don = ''
            else:
                try:
                    don = int(val)
                except:
                    don = float('%.5f' % float(val))
            nline.append(don)
        dico[i] = nline
        i += 1
    return dico, first



def read_dist_list(filein, sep=';') :
    ldict = {}
    with open(filein, 'r', newline='', encoding='utf8') as csvfile :
        csvreader = csv.reader(csvfile, delimiter=sep, quotechar='"')
        i = 0
        for row in csvreader :
            if i == 0 :
                first = row
            else :
                ldict[i-1] = row[1:]
            i+=1
    return ldict, first

def exec_RCMD(rpath, command) :
    log.info('R CMD INSTALL %s' % command)
    rpath = rpath.replace('\\','\\\\')
    error = call(["%s" % rpath, 'CMD', 'INSTALL', "%s" % command])
    return error

def exec_rcode(rpath, rcode, wait = True, graph = False):
    log.info("R Script : %s" % rcode)
    needX11 = False
    if sys.platform == 'darwin' :
        try :
            macversion = platform.mac_ver()[0].split('.')
            if int(macversion[1]) < 5 :
                needX11 = True
            else :
                needX11 = False
        except :
            needX11 = False
    rpath = rpath.replace('\\','\\\\')
    env = os.environ.copy()
    if sys.platform == 'darwin' and 'LC_ALL' not in env:
        env['LC_ALL'] = 'en_US.UTF-8'
    if not graph :
        if wait :
            if sys.platform == 'win32':
                error = call(["%s" % rpath, "--vanilla","--slave","-f", "%s" % rcode])
            else :
                error = call([rpath, '--slave', "--vanilla", "--encoding=UTF-8", "-f %s" % rcode], env = env)
            return error
        else :
            if sys.platform == 'win32':
                pid = Popen(["%s" % rpath, '--vanilla','--slave','-f', "%s" % rcode])
            else :
                pid = Popen([rpath, '--slave', "--vanilla", "--encoding=UTF-8", "-f %s" % rcode], stderr = PIPE, env = env, encoding='UTF-8') #PIPE ou STDOUT ?
            return pid
    else :
        if wait :
            if sys.platform == 'win32':
                error = call(["%s" % rpath, '--vanilla','--slave','-f', "%s" % rcode])
            elif sys.platform == 'darwin' and needX11:
                os.environ['DISPLAY'] = ':0.0'
                error = call([rpath, '--vanilla','--slave', "--encoding=UTF-8","-f %s" % rcode], env = env, encoding='UTF-8')
            else :
                error = call([rpath, '--vanilla','--slave', "--encoding=UTF-8","-f %s" % rcode], env = env, encoding='UTF-8')
            return error
        else :
            if sys.platform == 'win32':
                pid = Popen(["%s" % rpath, '--vanilla','--slave','-f', "%s" % rcode])
            elif sys.platform == 'darwin' and needX11:
                os.environ['DISPLAY'] = ':0.0'
                pid = Popen([rpath, '--vanilla','--slave', "--encoding=UTF-8","-f %s" % rcode], stderr = PIPE, env = env, encoding='UTF-8')
            else :
                pid = Popen([rpath, '--vanilla','--slave', "--encoding=UTF-8","-f %s" % rcode], stderr = PIPE, env = env, encoding='UTF-8')
            return pid

def check_Rresult(parent, pid) :
    if isinstance(pid, Popen) :
        if pid.returncode != 0 :
            error = pid.communicate()
            error = [str(error[0]), error[1]]
            if error[1] is None :
                error[1] = 'None'
            parent.Rerror = '\n'.join([str(pid.returncode), '\n'.join(error)])
            try :
                raise Exception('\n'.join(['Erreur R', '\n'.join(error[1:])]))
            except :
                BugReport(parent)
            return False
        else :
            return True
    else :
        if pid != 0 :
            try :
                raise Exception('Erreur R')
            except :
                BugReport(parent)
            return False
        else :
            return True


def launchcommand(mycommand):
    Popen(mycommand)

def print_liste(filename,liste):
    with open(filename,'w', encoding='utf8') as f :
        for graph in liste :
            f.write(';'.join(graph) +'\n')

def read_list_file(filename, encoding = 'utf8'):
    with open(filename,'r', encoding='utf8') as f:
        content=f.readlines()
        ncontent=[line.replace('\n','').split(';') for line in content if line.strip() != '']
    return ncontent

def progressbar(self, maxi):
    ira = wx.GetApp().GetTopWindow()
    parent = ira
    try:
        maxi = int(maxi)
    except:
        maxi = 1
    prog = wx.ProgressDialog("Traitements",
                             "Veuillez patienter...",
                             maximum=maxi,
                             parent=parent,
                             style=wx.PD_APP_MODAL | wx.PD_AUTO_HIDE | wx.PD_ELAPSED_TIME | wx.PD_CAN_ABORT
                             )
                             # parent ???
    # le ABORT n'est pas géré à tous les coups ???
    prog.SetSize((400,150))
    #prog.SetIcon(ira._icon)
    return prog

def treat_var_mod(variables) :
    var_mod = {}
    variables = list(set(variables))
    varmod = [variable.split('_') for variable in variables]
    vars = list(set([var[0] for var in varmod if len(var) >=2]))
    for var in vars :
        mods = ['_'.join(v) for v in varmod if v[0] == var]
        var_mod[var] = mods

#     for variable in variables :
#         if '_' in variable :
#             forme = variable.split('_')
#             var = forme[0]
#             mod = forme[1]
#             if not var in var_mod :
#                 var_mod[var] = [variable]
#             else :
#                 if not mod in var_mod[var] :
#                     var_mod[var].append(variable)
    return var_mod

def doconcorde(corpus, uces, mots, uci = False, et = False, fontsize = 4) :
    if not uci :
        ucestxt1 = [row for row in corpus.getconcorde(uces)]
    else :
        ucestxt1 = [row for row in corpus.getuciconcorde(uces)]
    ucestxt1 = dict(ucestxt1)
    ucestxt = []
    ucis_txt = []
    if not et :
        listmot = [corpus.getlems()[lem].formes for lem in mots]
        listmot = [corpus.getforme(fid).forme for lem in listmot for fid in lem]
    else :
        listmot = mots
    mothtml = ['<font color=red><b>%s</b></font>' % mot for mot in listmot]
    dmots = dict(list(zip(listmot, mothtml)))
    presfont = '<p><b><font size="%i">' % fontsize
    font = '<font size="%i">' % fontsize
    for uce in uces :
        ucetxt = ucestxt1[uce].split()
        ucetxt = ' '.join([dmots.get(mot, mot) for mot in ucetxt])
        if not uci :
            uciid = corpus.getucefromid(uce).uci
            ucis_txt.append(presfont + ' '.join(corpus.ucis[corpus.getucefromid(uce).uci].etoiles) + '<a href="%i_%i"> *%i_%i</a></font></b></p>' % (uciid, uce, uciid, uce))
        else :
            ucis_txt.append(presfont + ' '.join(corpus.ucis[uce].etoiles) + '</font></b></p>')
        ucestxt.append(font + ucetxt + '</font>')
    return ucis_txt, ucestxt


def getallstcarac(corpus, analyse) :
   pathout = PathOut(analyse['ira'])
   profils =  ReadProfileAsDico(pathout['PROFILE_OUT'], Alceste, 'utf8')
   print(profils)

def read_chd(filein, fileout):
    with open(filein, 'r', encoding='utf8') as f :
        content = f.read()
    #content = [line[3:].replace('"',"").replace(' ','') for line in content.splitlines()]
    content = [line.split('\t') for line in content.splitlines()]
    chd = {'name':1, 'children':[]}
    mere={}
    for i, line in enumerate(content) :
        if i == 0 :
            chd['children'] = [{'name': line[1],'size' : content[i+1][0]}, {'name':line[2], 'size': content[i+1][1]}]
            mere[line[1]] = chd['children'][0]
            mere[line[2]] = chd['children'][1]
        elif not i % 2 :
            if 'children' in mere[line[0]]:
                mere[line[0]]['children'].append({'name': line[1],'size' : content[i+1][0]})
                mere[line[1]] = mere[line[0]]['children'][-1]
                mere[line[0]]['children'].append({'name': line[2],'size' : content[i+1][1]})
                mere[line[2]] = mere[line[0]]['children'][-1]
            else :
                mere[line[0]]['children'] = [{'name': line[1],'size' : content[i+1][0]}, {'name':line[2], 'size': content[i+1][1]}]
                mere[line[1]] = mere[line[0]]['children'][-2]
                mere[line[2]] = mere[line[0]]['children'][-1]
    with open(fileout, 'w', encoding='utf8') as f :
        f.write(json.dumps(chd))


translation_languages = {"Afrikaans":"af", "Albanian":"sq", "Amharic":"am", "Arabic":"ar", "Armenian":"hy", "Azeerbaijani":"az", "Basque":"eu", "Belarusian":"be", "Bengali":"bn", "Bosnian":"bs", "Bulgarian":"bg", "Catalan":"ca", "Cebuano":"ceb", "Chichewa":"ny", "Chinese (Simplified)":"zh-CN", "Chinese (Traditional)":"zh-TW", "Corsican":"co", "Croatian":"hr", "Czech":"cs", "Danish":"da", "Dutch":"nl", "English":"en", "Esperanto":"eo", "Estonian":"et", "Filipino":"tl", "Finnish":"fi", "French":"fr", "Frisian":"fy", "Galician":"gl", "Georgian":"ka", "German":"de", "Greek":"el", "Gujarati":"gu", "Haitian Creole":"ht", "Hausa":"ha", "Hawaiian":"haw", "Hebrew":"iw", "Hindi":"hi", "Hmong":"hm", "Hungarian":"hu", "Icelandic":"is", "Igbo":"ig", "Indonesian":"id", "Irish":"ga", "Italian":"it", "Japanese":"ja", "Javanese":"jw", "Kannada":"kn", "Kazakh":"kk", "Khmer":"km", "Korean":"ko", "Kurdish":"ku", "Kyrgyz":"ky", "Lao":"lo", "Latin":"la", "Latvian":"lv", "Lithuanian":"lt", "Luxembourgish":"lb", "Macedonian":"mk", "Malagasy":"mg", "Malay":"ms", "Malayalam":"ml", "Maltese":"mt", "Maori":"mi", "Marathi":"mr", "Mongolian":"mn", "Burmese":"my", "Nepali":"ne", "Norwegian":"no", "Pashto":"ps", "Persian":"fa", "Polish":"pl", "Portuguese":"pt", "Punjabi":"ma", "Romanian":"ro", "Russian":"ru", "Samoan":"sm", "Scots Gaelic":"gd", "Serbian":"sr", "Sesotho":"st", "Shona":"sn", "Sindhi":"sd", "Sinhala":"si", "Slovak":"sk", "Slovenian":"sl", "Somali":"so", "Spanish":"es", "Sundanese":"su", "Swahili":"sw", "Swedish":"sv", "Tajik":"tg", "Tamil":"ta", "Telugu":"te", "Thai":"th", "Turkish":"tr", "Ukrainian":"uk", "Urdu":"ur", "Uzbek":"uz", "Vietnamese":"vi", "Welsh":"cy", "Xhosa":"xh", "Yiddish":"yi", "Yoruba":"yo", "Zulu":"zu", }


def gettranslation(words, lf, lt) :
    import urllib.request, urllib.error, urllib.parse
    import json
    agent = {'User-Agent':
    "Mozilla/4.0 (\
    compatible;\
    MSIE 6.0;\
    Windows NT 5.1;\
    SV1;\
    .NET CLR 1.1.4322;\
    .NET CLR 2.0.50727;\
    .NET CLR 3.0.04506.30\
    )"}
    base_link = "https://translate.googleapis.com/translate_a/single?client=gtx&sl=%s&tl=%s&dt=t&q=%s"
    print(len(words))
    totrans = urllib.parse.quote('\n'.join(words))
    link = base_link % (lf, lt, totrans)
    request = urllib.request.Request(link, headers=agent)
    raw_data = urllib.request.urlopen(request).read()
    data = json.loads(raw_data)
    return [line[0].replace("'", '_').replace(' | ', '|').replace(' ', '_').replace('-','_').replace('\n','') for line in data[0]]

def makenprof(prof, trans, deb=0) :
    nprof=[]
    if deb == 0 :
        nprof.append(prof[0])
    for i, val in enumerate(trans) :
        line = prof[deb+i+1][:]
        line[6] = val
        nprof.append(line)
    return nprof

def treatempty(val) :
    if val.strip() == '' :
        return '_'
    else :
        return val

def translateprofile(corpus, dictprofile, lf='it', lt='fr', maxword = 50) :
    nprof = {}
    lems = {}
    for i in range(len(dictprofile)) :
        prof = dictprofile[repr(i+1)]
        try :
            lenact = prof.index(['*****', '*', '*', '*', '*', '*', '', ''])
            lensup = -1
        except ValueError:
            try :
                lenact = prof.index(['*', '*', '*', '*', '*', '*', '', ''])
                lensup = 0
            except ValueError:
                lenact = len(prof)
                lensup = 0
        try :
            lensup += prof.index(['*', '*', '*', '*', '*', '*', '', ''])
            lensup = lensup - lenact
        except ValueError:
            lensup += len(prof) - lenact
        if lenact != 0 :
            if lenact > maxword :
                nlenact = maxword
            else :
                nlenact = lenact
            actori = [line[6] for line in prof[1:nlenact]]
            act = [val.replace('_', ' ') for val in actori]
            act = gettranslation(act, lf, lt)
            for j, val in enumerate(actori) :
                if act[j] not in lems :
                    lems[act[j]] = val
                else :
                    while act[j] in lems :
                        act[j] = act[j] + "+"
                    lems[act[j]] = val
            nprof[repr(i+1)] = makenprof(prof, act)

        if lensup != 0 :
            if lensup > maxword :
                nlensup = maxword
            else :
                nlensup = lensup
            supori = [line[6] for line in prof[(1+lenact):(lenact+nlensup)]]
            sup = [val.replace('_', ' ') for val in supori]
            sup = [treatempty(val) for val in sup]
            sup = gettranslation(sup, lf, lt)
            for j, val in enumerate(supori) :
                if sup[j] not in lems :
                    lems[sup[j]] = val
                else :
                    while sup[j] in lems :
                        sup[j] = sup[j] + "+"
                    lems[sup[j]] = val
            nprof[repr(i+1)].append(['*****', '*', '*', '*', '*', '*', '', ''])
            nprof[repr(i+1)] += makenprof(prof, sup, deb=lenact)

        try :
            lenet = prof.index(['*', '*', '*', '*', '*', '*', '', ''])
            nprof[repr(i+1)].append(['*', '*', '*', '*', '*', '*', '', ''])
            nprof[repr(i+1)] += prof[(lenet+1):]
        except :
            pass
    return nprof, lems

def write_translation_profile(prof, lems, language, dictpathout) :
    if os.path.exists(dictpathout['translations.txt']) :
        with open(dictpathout['translations.txt'], 'r', encoding='utf8') as f :
            translist = f.read()
        translist = [line.split('\t') for line in translist.splitlines()]
    else :
        translist = []
    toprint = []
    toprint.append(['','','','','',''])
    toprint.append(['***', 'nb classes', repr(len(prof)), '***', '', ''])
    for i in range(len(prof)) :
        toprint.append(['**', 'classe', repr(i+1), '**', '', ''])
        toprint.append(['****'] + prof[repr(i+1)][0] + ['****'])
        rest = [[repr(line[1]), repr(line[2]), repr(line[3]), repr(line[4]), line[6], line[7].replace('< 0,0001', '0.00009').replace('NS (','').replace(')','')] for line in prof[repr(i+1)][1:]]
        for i, line in enumerate(prof[repr(i+1)][1:]) :
            if line[0] == '*' :
                rest[i] = ['*', '*', '*', '*', '*', '*']
            elif line[0] == '*****' :
                rest[i] = ['*****','*','*', '*', '*', '*']
        toprint += rest
    with open(dictpathout['translation_profile_%s.csv' % language], 'w', encoding='utf8') as f :
        f.write('\n'.join([';'.join(line) for line in toprint]))
    with open(dictpathout['translation_words_%s.csv' % language], 'w', encoding='utf8') as f :
        f.write('\n'.join(['\t'.join([val, lems[val]]) for val in lems]))
    if 'translation_profile_%s.csv' % language not in [val[0] for val in translist] :
        translist.append(['translation_profile_%s.csv' % language, 'translation_words_%s.csv' % language])
        with open(dictpathout['translations.txt'], 'w', encoding='utf8') as f :
            f.write('\n'.join(['\t'.join(line) for line in translist]))

def makesentidict(infile, language) :
    with codecs.open(infile,'r', 'utf8') as f :
        content = f.read()
    content = [line.split('\t') for line in content.splitlines()]
    titles = content.pop(0)
    senti = ['Positive', 'Negative', 'Anger', 'Anticipation', 'Disgust', 'Fear', 'Joy', 'Sadness', 'Surprise', 'Trust']
    sentid = {}
    for sent in senti :
        sentid[sent] = titles.index(sent)
    frtitle = [val for val in titles if '(fr)' in val]
    frid = titles.index(frtitle[0])
    sentidict = [[line[frid].lower(), [line[sentid[sent]] for sent in senti]] for line in content]
    pos = ['positive'] + [line[0] for line in sentidict if line[1][0] == '1']
    neg = ['negative'] + [line[0] for line in sentidict if line[1][1] == '1']
    anger = ['anger'] + [line[0] for line in sentidict if line[1][2] == '1']
    anticipation = ['anticipation'] + [line[0] for line in sentidict if line[1][3] == '1']
    disgust = ['disgust'] + [line[0] for line in sentidict if line[1][4] == '1']
    fear = ['fear'] + [line[0] for line in sentidict if line[1][5] == '1']
    joy = ['joy'] + [line[0] for line in sentidict if line[1][6] == '1']
    sadness = ['sadness'] + [line[0] for line in sentidict if line[1][7] == '1']
    surprise = ['surprise'] + [line[0] for line in sentidict if line[1][8] == '1']
    trust = ['trust'] + [line[0] for line in sentidict if line[1][9] == '1']
    with open('/tmp/tgenemo.csv', 'w') as f :
        for val in [pos, neg, anger, anticipation, disgust, fear, joy, sadness, surprise, trust] :
            f.write('\t'.join(val) + '\n')

def countsentfromprof(prof, encoding, sentidict) :
    with codecs.open(prof, 'r', encoding) as f :
        content = f.read()
    content = [line.split(';') for line in content.splitlines()]
    print(content)
    content = [[line[0], [int(val) for val in line[1:]]] for line in content]
    print(content)
    content = dict(content)
    print(content)

def iratolexico(infile, outfile, encoding) :
    with codecs.open(infile, 'r', encoding) as f :
        for line in f :
            if line.startswith('**** ') :
                line = line.split()

