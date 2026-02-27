# -*- coding: utf-8 -*-
#Author: Pierre Ratinaud
#Copyright (c) 2008-2020 Pierre Ratinaud
#modification pour python 3 : Laurent Mérat, 6x7 - mai 2020
#License: GNU/GPL

#------------------------------------
# import des modules python
#------------------------------------
import codecs
import os

#------------------------------------
# import des modules wx
#------------------------------------
import wx

#------------------------------------
# import des fichiers du projet
#------------------------------------
from dialog import ExtractDialog
from corpus import Corpus, copycorpus


parametres = {'filein' : 'corpus/lru2.txt',
              'encodein' : 'utf8',
              'encodeout' : 'utf8',
              'mods' : ['*annee_2010', '*annee_2011']}


def istext(line) :
    if line.startswith('**** ') :
        return True
    else :
        return False

def isthem(line):
    if line.startswith('-*') :
        return True
    else :
        return False

def testvar(line, variable) :
    line = line.split()
    varmod = [val.split('_') for val in line[1:]]
    vars = [var[0] for var in varmod]
    if variable in vars :
        return '_'.join([variable, varmod[vars.index(variable)][1]]).replace('*','')
    else :
        return False

def testmod(line, mods) :
    line = line.split()
    for mod in mods :
        if mod in line[1:] :
            return mod.replace('*','')
    return False


class Extract :

    def __init__(self, parent, option) :
        dial = ExtractDialog(parent, option)
        dial.CenterOnParent()
        res = dial.ShowModal()
        if res == wx.ID_OK :
            parametres = dial.make_param()
            if option == 'splitvar' :
                SplitFromVar(parametres)
            elif option == 'mods' :
                ExtractMods(parametres)
            elif option == 'them' :
                SplitFromThem(parametres)
            dial.Destroy()
            dial = wx.MessageDialog(parent, 'Done !', style = wx.OK)
            dial.ShowModal()
            dial.Destroy()
        else :
            dial.Destroy()


class SplitFromVar :

    def __init__(self, parametres) :
        self.filein = parametres['filein']
        self.var = parametres['var']
        self.encodein = parametres['encodein']
        self.encodeout = parametres['encodeout']
        self.basepath = os.path.dirname(self.filein)
        self.doparse()

    def doparse(self) :
        keepline = False
        filedict = {}
        with codecs.open(self.filein, 'r', self.encodein) as fin :
            for line in fin :
                if istext(line) :
                    varmod = testvar(line, self.var)
                    if varmod :
                        keepline = True
                        if varmod not in filedict :
                            filename = os.path.join(self.basepath, varmod + '.txt')
                            filedict[varmod] = open(filename, 'w')
                        fileout = filedict[varmod]
                    else : 
                        keepline = False
                if keepline :
                    fileout.write(line)
        for f in filedict :
            filedict[f].close()


class SplitFromThem :

    def __init__(self, parametres) :
        self.filein = parametres['filein']
        self.them = parametres['them']
        self.encodein = parametres['encodein']
        self.encodeout = parametres['encodeout']
        self.basepath = os.path.dirname(self.filein)
        self.pathout = os.path.join(self.basepath, '_'.join([them.replace('-*','') for them in self.them]))
        self.fileout = open(self.pathout, 'w')
        self.doparse()
        self.fileout.close()

    def doparse(self):
        text = ''
        keepline = False
        lastet = ''
        with codecs.open(self.filein, 'r', self.encodein) as fin :
            for line in fin :
                if istext(line) :
                    self.writetext(self.fileout, lastet, text)
                    text = ''
                    lastet = line
                if isthem(line) :
                    l = line.strip().rstrip('\n\r')
                    if l in self.them :
                        keepline = True
                    else :
                        keepline = False
                if keepline :
                    text += line
            self.writetext(self.fileout, lastet, text)

    def writetext(self, fileout, lastet, text):
        if text != '' :
            self.fileout.write(lastet + text)


class ExtractMods :

    def __init__(self, parametres) :
        self.onefile = parametres.get('onefile', False)
        self.filein = parametres['filein']
        self.mods = parametres['mods']
        self.encodein = parametres['encodein']
        self.encodeout = parametres['encodeout']
        self.basepath = os.path.dirname(self.filein)
        if self.onefile :
            filename = os.path.join(self.basepath, '_'.join([mod.replace('*','') for mod in self.mods])+'.txt')
            self.fileout = open(filename, 'w')
        self.doparse()

    def doparse(self) :
        keepline = False
        filedict = {}
        with codecs.open(self.filein, 'r', self.encodein) as fin :
            for line in fin :
                if istext(line) :
                    modinline = testmod(line, self.mods)
                    if modinline :
                        keepline = True
                        if not self.onefile :
                            if modinline not in filedict :
                                filename = os.path.join(self.basepath, modinline + '.txt')
                                filedict[modinline] = open(filename, 'w')
                            fileout = filedict[modinline]
                        else :
                            fileout = self.fileout
                    else : 
                        keepline = False
                if keepline :
                    fileout.write(line)
        if not self.onefile :
            for f in filedict :
                filedict[f].close()
        else :
            self.fileout.close()


class SubCorpus(Corpus) :

    def __init__(self, parent, corpus, sgts) :
        Corpus.__init__(self, parent, corpus.parametres)
        self.sgts = sgts
        self.corpus = copycorpus(corpus)
        self.corpus.make_lems(self.parametres['lem'])
        textes = list(set([corpus.getucefromid(sgt).uci for sgt in sgts]))
        self.ucis = [corpus.ucis[i] for i in textes]
        for texte in self.ucis :
            texte.uces = [uce for uce in texte.uces if uce.ident in self.sgts] 
        self.make_formes(corpus)
        self.pathout = corpus.pathout 
        self.parametres['sub'] = self.sgts

    def make_formes(self, corpus) :
        self.formes = {}
        for forme in self.corpus.formes :
            sgtseff = self.corpus.getformeuceseff(forme)
            sgts = set(self.sgts).intersection(list(sgtseff.keys()))
            if len(sgts) :
                self.formes[forme] = self.corpus.formes[forme]
                self.formes[forme].freq = sum([sgtseff[sgt] for sgt in sgts])

    def getlemuces(self, lem) :
        return list(set(self.sgts).intersection(self.corpus.getlemuces(lem)))


def converttabletocorpus(table, fileout, enc='UTF8') :
    var = table.pop(0)
    var = var[0:len(var)-1]
    print(var)
    et = [list(zip(var, line[0:len(line)-1])) for line in table]
    et = ['**** ' + ' '.join(['*' + '_'.join(val) for val in line]) for line in et] 
    txt = ['\n'.join([et[i], line[-1]]) for i, line in enumerate(table)]
    print('\n'.join(txt))
    #with open(fileout, 'w') as f :


# execution directe ???
if __name__ == '__main__' :
    #SplitFromVar(parametres)
    ExtractMods(parametres, True)
