# -*- coding: utf-8 -*-
#modification pour python 3 : Laurent Mérat, 6x7 - mai 2020

#------------------------------------
# import des modules python
#------------------------------------
import codecs
from shutil import copyfile
import os

#------------------------------------
# import des fichiers du projet
#------------------------------------
from chemins import PathOut


class WebExport :

    def __init__(self, parent, parametres) :
        self.parent = parent
        self.parametres = parametres
        self.jspathin = os.path.join(self.parent.AppliPath, 'webexport','js')
        self.csspathin = os.path.join(self.parent.AppliPath, 'webexport', 'css')
        self.imgpathin = os.path.join(self.parent.AppliPath, 'webexport', 'images')
        if not 'dirout' in self.parametres :
            self.pathout = PathOut(filename = self.parametres['gexffile'], analyse_type='webexport')
            dirout = self.pathout.mkdirout()
            self.pathout.dirout = dirout
        else :
            self.pathout = PathOut(dirout = self.parametres['dirout'])
        self.makedirout()

    def makedirout(self) :
        jss = os.listdir(self.jspathin)
        css = os.listdir(self.csspathin)
        img = os.listdir(self.imgpathin)
        jspathout = os.path.join(self.pathout.dirout, 'js')
        cssout =  os.path.join(self.pathout.dirout, 'css')
        imgout = os.path.join(self.pathout.dirout, 'images')
        os.makedirs(jspathout)
        os.makedirs(cssout)
        os.makedirs(imgout)
        for f in jss :
            copyfile(os.path.join(self.jspathin, f), os.path.join(jspathout, f))
        for f in css :
            copyfile(os.path.join(self.csspathin, f), os.path.join(cssout, f))
        for f in img :
            copyfile(os.path.join(self.imgpathin, f), os.path.join(imgout, f))

    def exportafc(self) :
        copyfile(self.parametres['gexffile'], os.path.join(self.pathout.dirout, os.path.basename(self.parametres['gexffile'])))
        afcfile = os.path.join(self.parent.AppliPath, 'webexport', 'afc.html')
        afcout = os.path.join(self.pathout.dirout, 'afc.html')
        with codecs.open(afcfile, 'r', 'utf8') as f :
            content = f.read()
        self.parametres['gexffile'] = os.path.basename(self.parametres['gexffile'])
        content = content % self.parametres
        with open(afcout, 'w') as f :
            f.write(content)
        return afcout

    def exportsimi(self) :
        simifile = os.path.join(self.parent.AppliPath, 'webexport', 'graphe.html')
        simiout = os.path.join(self.pathout.dirout, 'graphe.html')
        with codecs.open(simifile, 'r', 'utf8') as f :
            content = f.read()
        self.parametres['gexffile'] = os.path.basename(self.parametres['gexffile'])
        content = content % self.parametres
        with open(simiout, 'w') as f :
            f.write(content)
        return simiout             
