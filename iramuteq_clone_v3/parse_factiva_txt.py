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
import re


mois = {'janvier' : '01', 
        'février' : '02',
        'mars' : '03',
        'avril' : '04', 
        'mai' : '05',
        'juin' : '06',
        'juillet' : '07',
        'août' : '08',
        'septembre' : '09',
        'octobre' : '10',
        'novembre' : '11',
        'décembre' : '12', 
        'january' : '01',
        'february': '02',
        'march' : '03',
        'april': '04',
        'may': '05',
        'june' : '06',
        'july': '07',
        'august': '08',
        'september' : '09',
        'october': '10',
        'november': '11',
        'december': '12'}


def parsetxtpaste(txt):
    """
    parser de texte pour factiva
    à partir d'un copier/coller de la fenêtre de visualisation
    merci à Lucie Loubère pour l'astuce :)
    """

    no = ['NS','RE','IPD','CO','IN']  # les balises qui signalent une fin
    txt = txt.splitlines()
    keepline = False
    ucis = []
    for line in txt : 
        if line.startswith('Article') :
            lp = line.split()
            if len(lp) > 2  :
                if lp[2] == 'Article' or lp[2] == 'Next' or lp[2] == 'Previous':
                    ucis.append([['****'],''])
                    keepline = False
        if line.startswith('SN ') : #source
            jsource = re.sub('[\'" !\.?;,:\+\-°&]', '', line[4:])
            source = '_'.join(['*source', jsource]).lower()
            #source = '*source_' + line[4:].replace(' ','').replace('\'','').replace('´','').replace('’','').replace('-','').lower()
            ucis[-1][0].append(source)
        elif line.startswith('PD ') : #date
            datemois = line[4:].split(' ')[1].lower()
            datemois = mois.get(datemois, datemois)
            dateannee = line[4:].split(' ')[2]
            datejour = '%02d' % int(line[4:].split(' ')[0])
            am = '_'.join(['*am', dateannee, datemois])
            amj = '_'.join(['*amj', dateannee, datemois, datejour])
            ucis[-1][0].append(am)
            ucis[-1][0].append(amj)
            annee = '_'.join(['*annee', dateannee])
            ucis[-1][0].append(annee)
        elif line.strip() in no : #fin
            keepline = False
        elif line.startswith('RF ') : #fin
            keepline = False
        elif line.strip() in ['LP', 'TD'] : #debut texte
            keepline = True
        else :
            pass
        if keepline and line.strip() not in ['LP', 'TD', ''] :
            ucis[-1][1] = '\n'.join([ucis[-1][1],line.replace('*', ' ')])
    return ucis

def print_ucis(ucis, ofile, encodage) :
    #elimination des articles vides
    ucis = [uci for uci in ucis if uci[1].strip() != '']
    toprint = '\n\n'.join(['\n'.join([' '.join(uci[0]),uci[1]]) for uci in ucis])
    ofile.write(toprint + '\n')


class ParseFactivaPaste :

    def __init__(self, txtdir, fileout, encodage_in, encodage_out) :
        files = []
        for root, subfolders, subfiles in os.walk(txtdir) :
            nf = [os.path.join(root, f) for f in subfiles if f.split('.')[-1] == 'txt']
            nf.sort()
            files += nf
        tot = 0
        with open(fileout,'w', encoding='utf8') as outf : 
            for f in files : 
                print(f)
                with codecs.open(f, 'r', encodage_in) as infile :
                    content = infile.read()
                ucis = parsetxtpaste(content)
                print_ucis(ucis, outf, encodage_out)
                tot += len(ucis)
                print('ok', len(ucis), 'articles', ' - total : ', tot)

# execution en direct ???
if __name__ == '__main__' :
    doparse(txtdir, fileout, encodage_in, encodage_out)
    print('fini')
