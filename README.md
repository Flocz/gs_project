# gs_projects


# ScriptLaMet_CarnGrat_AdB.r
# Script de traitement de données individuelles d'un remplissage de Carnet de Gratitude. le carnet d egratitude est uen # technique de psychologie positive permettant de développer les ressources de patients/clients qui l'utilisent. Dans la présente version, il consiste en la saaisie journalière de son état de bien-être (échelle subjective de 0 à 10) et la saisie de 3 motifs de gratitudes (champs, données textuelles). Le principe est d'entrainer l'attention du patient/client à repérer des motifs de gratitude de façon journalière. L'idée est que cet apprentissage affecte positivement son humeur sur le moment et durablement. En outre, il permet de situer quelles sont les ressources du client/patient.
# Le script fait plusieurs opérations:
# (1) aller chercher des données recueillies online sur Google Drive
# (2) mesurer différents paramètres basiques, dont l'évolution de l'état de bien être au fil du temps pour un individu donné
# (3) créer des nuages de mots résumant les motifs de gratitude d'un individu donné (après avoir mis en forme, nettoyé et caractérisé les motifs de gratitude en tant que données textuelles)
# (4) créer un graphique relationnel des mots pour voir les connexions (co-occurence) pour un individu donné (après avoir mis en forme les données, notamment sous forme de matrice d'adjacence)

# projets pour la suite
# (5) analyse des sentiments à l'aide d'un dictionnaire des sentiments: quels sentiments positifs vs négatifs?
# (6) représentation graphique conjointe des nuages de mots (fréquences) et de leurs relations avec prise en compte de la variable "etat de bien être": idée de voir si les mots les plus fréquents sont plus ou moins associé à une grande valeur de bien être ou non
# (7) comparaison des motifs de gratitudes au fil du temps (tranche de temps ou linéaire); comparaison des motifs de gratitudes en fonction de leur place (i.e. premier vs deuxième vs troisième motif, avec l'hypothèse que les premiers motifs sont plus faciles à mettre en évidence, plus fréquents, et les derniers plus rares, plus originaux, demandant plus d'effort mnésique et attentionnels)
# (8) analyse des thématiques de gratitude en fonction d'autres dictionnaires (?), par ex. en lien avec des domaines de besoins, ressources etc.
