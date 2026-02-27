# -*- coding: utf-8 -*-
#Author: Pierre Ratinaud
#Copyright (c) 2008-2020 Pierre Ratinaud
#modification pour python 3 : Laurent Mérat, 6x7 - mai 2020
#License: GNU/GPL

#------------------------------------
# import des modules python
#------------------------------------
import os
import codecs


def parsetxtmail(txt):
    """
    parser de texte pour factiva
    """

    no = ['NS','RE','IPD','CO','IN']  # les balises qui signalent une fin
    txt = txt.splitlines() #met le texte dans une liste de lignes
    txt.pop(0) # la premiere ligne sert a rien
    txt = txt[0:(len(txt)-10)] # les dernieres lignes ne servent a rien
    keepline = False
    ucis = []
    for line in txt : #pour chaque ligne du texte...
        if line.startswith('---------------------------------------------------------------') : # si la ligne commence avec...
            ucis.append([['****'],'']) # c'est une nouvelle uci
            keepline = False
        elif line.startswith('SN  ') : #source
            source = '*source_' + line[4:].replace(' ','').replace('\'','').replace('´','').replace('’','').replace('-','').lower()
            ucis[-1][0].append(source)
        elif line.startswith('PD ') : #date
            mois_annee = '*ma_' + line[4:].split(' ')[1] + line[4:].split(' ')[2]
            ucis[-1][0].append(mois_annee)
            annee = '*annee_' + line[4:].split(' ')[2]
            ucis[-1][0].append(annee)
        elif line in no : #fin
            keepline = False
        elif line.startswith('RF  ') : #fin
            keepline = False
        elif line in ['LP', 'TD'] : #debut texte
            keepline = True
        else :
            pass
        if keepline and line not in ['LP', 'TD'] :
            ucis[-1][1] = '\n'.join([ucis[-1][1],line])
    return ucis

def print_ucis(ucis, ofile, encodage) :
    ucis = [uci for uci in ucis if uci[1].strip() != '']
    toprint = '\n'.join(['\n'.join([' '.join(uci[0]),uci[1]]) for uci in ucis])
    ofile.write(toprint)


class ParseFactivaMail :

    def __init__(self, txtdir, fileout, encodage_in, encodage_out) :
        files = os.listdir(txtdir) #liste des fichiers dans txtdir
        with open(fileout,'w') as outf : #ouverture du fichier en sortie
            for f in files : #pour chaque fichier en entree...
                f= os.path.join(txtdir, f) #chemin du fichier
                with codecs.open(f, 'r', encodage_in) as infile : #ouverture du fichier
                    content = infile.read() #lecture du fichier
                ucis = parsetxtmail(content)
                print_ucis(ucis, outf, encodage_out)
