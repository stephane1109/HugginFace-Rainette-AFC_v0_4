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

#from BeautifulSoup import BeautifulSoup #???

#------------------------------------
# import des fichiers du projet
#------------------------------------
from html.parser import HTMLParser
from html import unescape


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


def finddate(data):
    data = data.split()
    try :
        day = int(data[0])
        year = int(data[2])
        month = mois[data[1]]
    except :
        return None
    else :
        return [repr(year), month, '%02d' % day]

def makedate(date):
    year = date[0:4]
    month = date[4:6]
    day = date[6:]
    return [year, month, day]


# create a subclass and override the handler methods
class MyHTMLParser(HTMLParser):

    def handle_starttag(self, tag, attrs):
        #print "Encountered a start tag:", tag
        if tag == 'span' :
            if len(attrs) > 0 :
                if attrs[0][1] == 'DocPublicationName' :
                    #print 'DocPublicationName'
                    self.headercount = 0
                    self.currentattr = 'DocPublicationName'
                elif attrs[0][1] == 'DocHeader' :
                    self.headercount += 1
                    self.currentattr = 'DocHeader'
                elif attrs[0][1] in ['TitreArticleVisu', 'titreArticleVisu', 'titreArticle'] :
                    self.outfile.write('\n\n')
                    self.meta.append('\n')
                    self.outfile.write(' '.join(self.meta))
                    self.meta = ['****']
                    self.nb += 1
                    self.currentattr = 'TitreArticleVisu'
                elif attrs[0][1] == 'PubliC_lblNodoc' :
                    self.currentattr = 'PubliC_lblNodoc'
        elif tag == 'table' :
            self.currentattr = None
        elif tag == 'div' :
            if len(attrs)>0 :
                if attrs[0][1] == 'publiC-lblNodoc' :
                    self.currentattr = 'PubliC_lblNodoc'
                elif attrs[0][1] == 'DocText' :
                    self.currentattr = 'TitreArticleVisu'
                elif attrs[0][1] == 'titreArticle' :
                    self.currentattr = 'TitreArticleVisu'
        elif tag == 'p' :
            if len(attrs) > 0 :
                if attrs[0][1] == 'titreArticleVisu' :
        #            self.outfile.write('\n\n')
        #            self.meta.append('\n')
        #            self.outfile.write(' '.join(self.meta).encode('utf8', errors='replace'))
        #            self.meta = ['****']
        #            self.nb += 1
                    self.currentattr = 'TitreArticleVisu'

    def handle_endtag(self, tag):
        pass
        #print "Encountered an end tag :", tag

    def handle_data(self, data):
        #print self.currentattr
        if self.currentattr == 'DocPublicationName' :
            #print data
            PublicationName = data.strip().replace(' ', '_').replace('(','').replace(')','').replace('-','').replace('.','').replace('/','').replace("'",'').replace(';', '').replace(':', '').replace('·','').lower()
            PublicationName = PublicationName.split(',')[0]
            if len([val for val in self.meta if val.startswith('*source_')]) == 0 :
                self.meta.append('*source_' + PublicationName)
            self.currentattr = None
#        elif self.currentattr == 'DocHeader' :
#            date = finddate(data)
#            if date is not None :
#                self.meta += ['*date_' + '-'.join(date), '*am_' + '-'.join(date[0:2]), '*annee_' + date[0]]
        elif self.currentattr == 'TitreArticleVisu' :
            #print data
            if data.startswith('©') :
                self.currentattr = None
                return
            self.content.append(' '.join(data.replace('\n', ' ').split()) + ' ')
            #self.outfile.write(' '.join(data.replace('\n', ' ').split()).encode('utf8', errors='replace') + ' ')
        elif self.currentattr == 'PubliC_lblNodoc' :
            date = data.split('·')[1]#data[5:13]
            date = makedate(date)
            self.meta += ['*date_' + '-'.join(date), '*am_' + '-'.join(date[0:2]), '*annee_' + date[0]]
            self.meta.append('\n')
            self.outfile.write('\n\n')
            self.outfile.write(' '.join(self.meta))
            self.outfile.write(' '.join(self.content))
            self.content = []
            self.meta = ['****']
            self.nb += 1
            self.currentattr = None

    def doinit(self, outfile):
        self.currentattr = None
        self.meta = ['****']
        self.content = []
        self.nb = 0
        self.outfile = outfile
        print('init ok')

def ParseEuropress(txtdir, fileout, encodage_in, encodage_out) :
    files = []
    if os.path.isdir(txtdir) :
        for root, subfolders, subfiles in os.walk(txtdir) :
            nf = [os.path.join(root, f) for f in subfiles if f.split('.')[-1] in ['html', 'HTML'] ]
            nf.sort()
            files += nf
        if len(files) == 0 :
            return 'nofile'
    elif os.path.isfile(txtdir) :
        files.append(txtdir)
    tot = 0
    parser = MyHTMLParser()
    with open(fileout,'w', encoding='utf8') as outf :
        for f in files :
            print(f)
            parser.doinit(outf)
            with codecs.open(f, 'r', encodage_in) as infile :
                content = infile.read()
                content = unescape(content)
            parser.feed(content)
            tot += parser.nb
    return tot
