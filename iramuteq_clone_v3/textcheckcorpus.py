# -*- coding: utf-8 -*-
#Author: Pierre Ratinaud
#Copyright (c) 2008-2020 Pierre Ratinaud
#modification pour python 3 : Laurent Mérat, 6x7 - mai 2020
#License: GNU/GPL

#------------------------------------
# import des modules wx
#------------------------------------
import wx
import wx.lib.dialogs

#------------------------------------
# import des fichiers du projet
#------------------------------------
from corpus import Corpus


def checkline(line, lnb) :
    if line.startswith('****') or (line[0:4].isdigit() and '*' in line) :
        if ' * ' in line :
            return [False, 1, lnb]
        else :
            lp = line.split()
            lp.pop(0)
            gv = [val for val in lp if not val.startswith('*') or ('-' in val)]
            if len(gv) != 0 :
                return [False, 4, lnb]
            else :
                return [True]
    elif line.startswith('-*') :
        if ' ' in line.strip() :
            return [False, 2, lnb]
        else :
            return [True]
    elif '*' in line :
        return [False, 3, lnb]
    else :
        return [True]


class checkcorpus :

    def __init__(self, parent):
        self.parent = parent
        self.corpus = Corpus(parent)
        self.corpus.parametre['encodage'] = parent.corpus_encodage
        self.corpus.parametre['filename'] = parent.filename
        self.corpus.content = parent.content
        res = [checkline(line, i) for i, line in enumerate(self.corpus.content.strip().splitlines())]
        res_nok = [val for val in res if not val[0]]
        if len(res_nok) != 0 :
            erreur_label = {1 : "une variable étoilée contient un espace",
                            2 : "une thématique contient un espace",
                            3 : "étoile dans ligne de texte",
                            4 : "une variable étoilée contient un espace ou un -"
                            }
            erreur = [['ligne %i' % val[2], erreur_label[val[1]]] for val in res_nok]
            txt = '\n----------------\n'.join(['\t'.join(line) for line in erreur])
            for val in res_nok :
                deb = self.parent.text_ctrl_txt.XYToPosition(0,val[2])
                fin = deb + self.parent.text_ctrl_txt.GetLineLength(val[2])
                self.parent.text_ctrl_txt.SetStyle(deb, fin, wx.TextAttr("#ff9c00", "#000000"))
            msg = "Veuillez corriger les fautes suivantes dans un éditeur de texte\npuis rechargez le corpus :\n\n" + txt
            win = wx.lib.dialogs.ScrolledMessageDialog(self.parent, msg, "Erreurs")
            win.CenterOnParent()
            win.ShowModal()
            win.Destroy()
        else :
            win = wx.MessageDialog(parent, "Pas de fautes !", "Corpus ok", wx.OK | wx.ICON_INFORMATION)
            win.CenterOnParent()
            win.ShowModal()
            win.Destroy()
