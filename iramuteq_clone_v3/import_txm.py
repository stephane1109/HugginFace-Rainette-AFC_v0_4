# -*- coding: utf-8 -*-
#Author: Pierre Ratinaud
#Copyright (c) 2008-2020 Pierre Ratinaud
#modification pour python 3 : Laurent Mérat, 6x7 - mai 2020
#License: GNU/GPL

#------------------------------------
# import des modules python
#------------------------------------
import os
import xml.sax
import glob

class TXMParser(xml.sax.ContentHandler) :
    def __init__(self, fileout, encodage_out) :
        self.fileout = fileout
        self.encodage_out = encodage_out
        self.sent = []

    def startElement(self, name, attrs) :
        self.name = name
        if name == 'title' :
            pass
        if name == 's' :
            pass
        if name == 'taxonomy' :
            pass
        if name == 'text' :
            self.text2stars(attrs)
        if name == 'w' :
            pass

    def endElement(self, name) :
        if name == 's' or name == 'w' :
            self.printsent()
        if name == 'p' :
            self.printsent()
            self.fileout.write('\n')

    def characters(self, content) :
        if self.name == 'txm:form' :
            if content not in ['', ' ', '\n', '\r'] :
                self.sent.append(content.rstrip('\n\r'))
            #self.fileout.write(content.encode('utf8'))

    def text2stars(self, attrs) :
        stars = ['_'.join(val).replace(' ', '_').replace("'", '_').replace('/','').replace('.','').replace(';', '').replace(':', '').replace('·','') for val in list(attrs.items())]
        stars = [''.join(['*', val]) for val in stars]
        stars = '**** ' + ' '.join(stars)
        self.fileout.write(stars)
        self.fileout.write('\n')

    def printsent(self) :
        if self.sent != [] :
            sent = ' ' + ' '.join(self.sent)
            for val in [' .', ' ,', ' ;', ' :', ' ?', ' !', ' -'] :
                sent = sent.replace(val, val.strip())
            sent = sent.replace("' ", "'")
            self.fileout.write(sent)
        self.sent = []

def TXM2IRA(pathin, fileout, encodage_in, encodage_out) :
        parser = xml.sax.make_parser()
        files = glob.glob(os.path.join(pathin,'*.xml'))
        if len(files) == 0 :
            return 'nofile'
        with open(fileout, 'w', encoding='utf8') as fout :
            parser.setContentHandler(TXMParser(fout, encodage_out))
            for f in files :
                parser.parse(open(f, 'r', encoding='utf8'))
                fout.write('\n\n')
        return None
