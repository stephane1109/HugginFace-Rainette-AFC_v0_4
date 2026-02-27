# -*- coding: utf-8 -*-
#Author: Pierre Ratinaud
#Copyright (c) 2008-2020 Pierre Ratinaud
#modification pour python 3 : Laurent Mérat, 6x7 - mai 2020
#License: GNU/GPL

"""
Removes HTML or XML character references and entities from a text string.

@param text The HTML (or XML) source text.
@return The plain text, as a Unicode string, if necessary.
"""

#------------------------------------
# import des modules python
#------------------------------------
import codecs
import sys
import xlrd
import ooolib
import os
from copy import copy
import re
import html.entities
#import shelve
import json
from uuid import uuid4
import logging

#------------------------------------
# import des fichiers du projet
#------------------------------------
from functions import DoConf
from chemins import PathOut


log = logging.getLogger('iramuteq.tableau')


def unescape(text):
    def fixup(m):
        #apos is not in the dictionnary
        html.entities.name2codepoint['apos'] = ord("'")
        text = m.group(0)
        if text[:2] == "&#":
            # character reference
            try:
                if text[:3] == "&#x":
                    return chr(int(text[3:-1], 16))
                else:
                    return chr(int(text[2:-1]))
            except ValueError:
                pass
        else:
            try:
                text = chr(html.entities.name2codepoint[text[1:-1]])
            except KeyError:
                pass
        return text # leave as is
    return re.sub("&#?\w+;", fixup, text)

def UpdateDico(Dico, word, line):
    if word in Dico :
        Dico[word][0] += 1
        Dico[word][1].append(line)
    else:
        Dico[word] = [1, [line]]

def copymatrix(tableau):
    log.info('copy matrix')
    copymat = Tableau(tableau.parent, parametres = tableau.parametres)
    copymat.linecontent = copy(tableau.linecontent)
    copymat.csvtable = copy(tableau.csvtable)
    copymat.pathout = copy(tableau.pathout)
    copymat.colnames = copy(tableau.colnames)
    copymat.rownb = copy(tableau.rownb)
    copymat.colnb = copy(tableau.colnb)
    if copymat.csvtable is None :
        copymat.open()
    return copymat


class Tableau() :

    def __init__(self, parent, filename = '', filetype = 'csv', encodage = 'utf-8', parametres = None) :
        self.parent = parent
        if parametres is None :
            self.parametres = DoConf(self.parent.ConfigPath['matrix']).getoptions('matrix')
            self.parametres['pathout'] = PathOut(filename, 'matrix').mkdirout()
            self.parametres['originalpath'] = filename
            self.parametres['filetype'] = filetype
            self.parametres['encodage'] = encodage
            #self.parametre['pathout'] = os.path.dirname(os.path.abspath(filename))
            self.parametres['mineff'] = 3
            self.parametres['syscoding'] = sys.getdefaultencoding()
            self.parametres['type'] = 'matrix'
            self.parametres['matrix_name'] = os.path.basename(filename)
            self.parametres['uuid'] = str(uuid4())
            self.parametres['shelves'] = os.path.join(self.parametres['pathout'], 'shelve')
            self.parametres['ira'] = os.path.join(self.parametres['pathout'], 'Matrix.ira')
        else :
            self.parametres = parametres
        self.pathout = PathOut(filename = filename, dirout = self.parametres['pathout'])
        self.csvtable = None
        self.sups = {}
        self.actives = {}
        self.listactives = None
        self.content = []
        self.linecontent = []
        self.isbinary = False
        self.binary = []
        self.firstrowiscolnames = True
        self.colnames = []
        self.firstcolisrownames = True
        self.rownames = []
        self.colnb = 0
        self.rownb = 0
        self.classes = []
        #self.parametres = self.parametre

    def read_tableau(self, fileout) :
        with open(fileout, 'r', encoding='utf8') as f :
            d = json.load(f)
        self.actives = d['actives']
        self.sups = d['sups']
        self.classes = d['classes']
        self.listactives = d['listactives']
        if 'listet' in d :
            self.listet = d['listet']
        if 'selected_col' in d :
            self.selected_col = d['selected_col']
        if 'datas' in d :
            self.datas = d['datas']
        if 'lchi' in d :
            self.lchi = d['lchi']
        if 'content' in d :
            self.content = d['content']

    def open(self):
        print('open matrix')
        self.read_csvfile()
        self.colnames = self.csvtable[0][1:]
        self.rownb = len(self.linecontent)
        self.colnb = len(self.linecontent[0])

    def save_tableau(self, fileout) :
        d = {}
        d['parametres'] = self.parametres
        d['actives'] = self.actives
        d['sups'] = self.sups
        d['classes'] = self.classes
        d['listactives'] = self.listactives
        if 'listet' in dir(self) :
            d['listet'] = self.listet
        if 'selected_col' in dir(self) :
            d['selected_col'] = self.selected_col
        if 'datas' in dir(self) :
            d['datas'] = self.datas
        if 'lchi' in dir(self) :
            d['lchi'] = self.lchi
        d['content'] = self.content
        with open(fileout, 'w', encoding='utf8') as f :
            json.dump(d, f)

    def make_content(self) :
        self.pathout.createdir(self.parametres['pathout'])
        if self.parametres['filetype'] == 'csv' :
            self.read_csv()
        elif self.parametres['filetype'] == 'xls' :
            self.read_xls()
        elif self.parametres['filetype'] == 'ods' :
            self.read_ods()
        self.parametres['csvfile'] = os.path.join(self.parametres['pathout'], 'csvfile.csv')
        self.make_tmpfile()
        DoConf().makeoptions(['matrix'],[self.parametres], self.parametres['ira'])
        self.parent.history.addMatrix(self.parametres)

    def make_content_simple(self):
        self.parametres['csvfile'] = os.path.join(self.parametres['pathout'], 'csvfile.csv')
        self.make_tmpfile()
        DoConf().makeoptions(['matrix'],[self.parametres], self.parametres['ira'])
        self.parent.history.addMatrix(self.parametres)

    def read_xls(self) :
        #FIXME : encodage
        #print '############## ENCODING IN EXCEL #######################'
        #datafile = xlrd.open_workbook(self.parametre['filename'], encoding_override="azerazerazer")
        datafile = xlrd.open_workbook(self.parametres['originalpath'])
        datatable = datafile.sheet_by_index(self.parametres['sheetnb']-1)
        self.linecontent = [[str(datatable.cell_value(rowx = i, colx = j)).replace('"','').replace(';',' ').replace('\n',' ').replace('\r', ' ').replace('\t', ' ').strip() for j in range(datatable.ncols)] for i in range(datatable.nrows)]

    def read_ods(self) :
        doc = ooolib.Calc(opendoc=self.parametres['originalpath'])
        doc.set_sheet_index(0)
        (cols, rows) = doc.get_sheet_dimensions()
        for row in range(1, rows + 1):
            ligne = []
            for col in range(1, cols + 1):
                data = doc.get_cell_value(col, row)
                if data is not None :
                    ligne.append(unescape(data[1].replace('"','').replace(';',' ').replace('\n', ' ').replace('\t', ' ').strip()))
                else :
                    ligne.append('')
            self.linecontent.append(ligne)

    def read_csv(self) :
        with codecs.open(self.parametres['originalpath'], 'r', self.parametres['encodage']) as f :
            content = f.read()
        self.linecontent = [line.split(self.parametres['colsep']) for line in content.splitlines()]
        self.linecontent = [[val.replace('"','').replace(';',' ').replace('\t', ' ').strip() for val in line] for line in self.linecontent]

    def write_csvfile(self) :
        with open(self.parametres['csvfile'], 'w', encoding='utf8') as f :
            f.write('\n'.join(['\t'.join(line) for line in self.csvtable]))

    def make_tmpfile(self) :
        self.rownb = len(self.linecontent)
        self.colnb = len(self.linecontent[0])
        if self.firstrowiscolnames :
            self.colnames = self.linecontent[0]
            self.linecontent.pop(0)
            self.rownb -= 1
        else :
            self.colnames = ['_'.join(['colonne', repr(i)]) for i in range(self.colnb)]
        if self.firstcolisrownames :
            self.rownames = [row[0] for row in self.linecontent]
            self.linecontent = [row[1:] for row in self.linecontent]
            self.colnb -= 1
            self.idname = self.colnames[0]
            self.colnames.pop(0)
            self.check_rownames()
        else :
            self.rownames = [repr(i) for i in range(self.rownb)]
            self.idname = 'identifiant'
        self.csvtable = [[self.idname] + self.colnames] + [[self.rownames[i]] + self.linecontent[i] for i in range(len(self.rownames))]
        self.write_csvfile()

    def read_csvfile(self):
        with open(self.parametres['csvfile'], 'r', encoding='utf8') as f:
            self.csvtable = [line.split('\t') for line in f.read().splitlines()]
        self.linecontent = [line[1:] for line in self.csvtable]
        self.linecontent.pop(0)

    def extractfrommod(self, col, val):
        return ([''] + self.colnames) + [line for line in self.csvtable[1:] if line[col + 1] == val]

    def splitfromvar(self, col):
        newtabs = {}
        for line in self.csvtable[1:] :
            mod = line[col+1]
            if mod in newtabs :
                newtabs[mod].append(line)
            else :
                newtabs[mod] = [line]
        for mod in newtabs :
            newtabs[mod].insert(0, [''] + self.colnames)
        return newtabs

    def check_rownames(self) :
        if len(self.rownames) == len(list(set(self.rownames))) :
            print('row names ok')
        else :
            print('les noms de lignes ne sont pas uniques, ils sont remplaces')
            self.rownames = [repr(i) for i in range(self.rownb)]

    def make_unique_list(self) :
        return list(set([val for line in self.linecontent for val in line if val.strip() != '']))

    def make_dico(self, selcol) :
        dico = {}
        for i, line in enumerate(selcol) :
            for forme in line:
                if forme.strip() != '' :
                    UpdateDico(dico, forme, i)
        return dico

    def select_col(self, listcol) :
        dc = dict(list(zip(listcol, listcol)))
        selcol = [[val for i, val in enumerate(row) if i in dc] for row in self.linecontent]
        return selcol

    def countmultiple(self, liscol):
        return self.make_dico(self.select_col(liscol))

    def getactlistfromselection(self, listact) :
        selcol = self.select_col(listact)
        self.actives = self.make_dico(selcol)
        return [[val, self.actives[val][0]] for val in self.actives]

    def make_listactives(self) :
        self.listactives = [val for val in self.actives if val != 'NA' and self.actives[val][0] >= self.parametres['mineff']]

    def write01(self, fileout, dico, linecontent) :
        if self.listactives is None :
            self.listactives = [val for val in dico if val != 'NA' and dico[val][0] >= self.parametres['mineff']]
        out = [['0' for forme in self.listactives] for line in linecontent]
        for i, forme in enumerate(self.listactives) :
            for line in dico[forme][1] :
                out[line][i] = '1'
        #out = [[self.rownames[i]] + out[i] for i in range(len(linecontent))]
        #out.insert(0,[self.idname] + self.listactives)
        out.insert(0, self.listactives)
        with open(fileout, 'w', encoding='utf8') as f :
            f.write('\n'.join([';'.join(line) for line in out]))

    def make_01_from_selection(self, listact, listsup = None, dowrite = True) :
        selcol = self.select_col(listact)
        self.actives = self.make_dico(selcol)
        self.write01(self.pathout['mat01.csv'], self.actives, selcol)
        if listsup is not None :
            selcol = self.select_col(listsup)
            self.sups = self.make_dico(selcol)

    def make_01_alc_format(self, fileout) :
        for i, ligne in enumerate(self.linecontent) :
            for forme in ligne:
                if len(forme) >= 1:
                    if forme[0] == '*':
                        UpdateDico(self.sups, forme, i)
                    else:
                        UpdateDico(self.actives, forme, i)
        self.listactives = [val for val in self.actives if self.actives[val][0] >= self.parametres['mineff']]
        table = [['0' for i in range(len(self.listactives))] for j in range(self.rownb)]
        for i, val in enumerate(self.listactives) :
            for j, line in enumerate(self.linecontent) :
                if val in line :
                    table[j][i] = '1'
        #table = [[self.rownames[i]] + table[i] for i in range(len(self.rownames))]
        #table.insert(0, [self.idname] + self.listactives)
        table.insert(0, self.listactives)
        with open(fileout, 'w', encoding='utf8') as f:
            f.write('\n'.join([';'.join(line) for line in table]))

    def printtable(self, filename, Table, sep = ';'):
        with open(filename, 'w', encoding='utf8') as f :
            f.write('\n'.join([sep.join(line) for line in Table]))

    def buildprofil(self) :
        with open(self.pathout['uce'], 'r', encoding='utf8') as filein :
            content = filein.readlines()
        content.pop(0)
        lsucecl = []
        dicocl = {}
        for i, line in enumerate(content) :
            line = line.replace('\n', '').replace('"', '').split(';')
            UpdateDico(dicocl, line[1], i)
            lsucecl.append([int(line[0]) - 1, int(line[1])])
        self.classes = lsucecl
        nlist = [[nbuce, cl] for nbuce, cl in lsucecl if cl != 0]
        self.ucecla = len(nlist)
        if '0' in dicocl :
            self.clnb = len(dicocl) - 1
        else:
            self.clnb = len(dicocl)
        tablecont = []
        for active in self.listactives :
            line = [active]
            line0 = [0] * self.clnb
            line += line0
            for i in range(0, self.clnb) :
                for uce, cl in nlist:
                    if cl == i + 1 :
                        if active in self.linecontent[uce]:
                            line[i + 1] += 1
            if sum(line[1:]) > self.parametres['mineff']:
                tablecont.append([line[0]] + [repr(don) for don in line if type(don) == type(1)])
        tablecontet = []
        for sup in self.sups :
            line = [sup]
            line0 = [0] * self.clnb
            line += line0
            for i in range(0, self.clnb) :
                for uce, cl in nlist:
                    if cl == i + 1 :
                        if sup in self.linecontent[uce]:
                            line[i + 1] += 1
            tablecontet.append([line[0]] + [repr(don) for don in line if type(don) == type(1)])

        self.printtable(self.pathout['ContEtOut'], tablecontet)
        self.printtable(self.pathout['Contout'], tablecont)

    def get_colnames(self) :
        return self.colnames[:]

    def make_table_from_classe(self, cl, la) :
        ln = [line[0] for line in self.classes if line[1] == cl]
        out = [['0' for col in la] for line in ln]
        for i, act in enumerate(la) :
            for j, line in enumerate(ln) :
                if line in self.actives[act][1] :
                    out[j][i] = '1'
        out.insert(0,[act for act in la])
        return out
