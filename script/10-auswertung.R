########################################
# Auswertung der Vorträge der SST2021 nach Sprache
########################################

library(tidyverse)

source("theme-sst21.R")

filedateprefix <- format(Sys.time(), "%Y%m%d")
figdirprefix <- '../figs/'

dat <- read_tsv(file='../csv/sst21stats.csv',
								skip=0,
								comment="#",
								col_types=cols(Tag = "i", 
															 ID="f",
															 Raum="f",
															 Vortragstyp = "f",
															 Sprache_Titel ="f",
															 Sprache_Folien = "f",
															 Sprache_Vortrag = "f",
															 Sprache_Moderation = "f",
															 Dauer_plan_min = "i",
															 Dauer_real_min = "i"
															 )
	)


dflang <- dat %>%
	mutate(
				 sprache_vortrag_folien_identisch = ifelse(as.character(Sprache_Folien) == as.character(Sprache_Vortrag), TRUE, FALSE),
				 dauer_delta = Dauer_real_min - Dauer_plan_min,
				 atelier_sonstige = as.factor(ifelse(Vortragstyp=="Atelier", "Atelier", "Sonstige")),
				 Sprache_Vortrag = fct_relevel(Sprache_Vortrag, "DE", "EN", "FR", "DE_FR", "IT")
				 )



##############################
# Auswertung Anzahl Vorträge pro Sprache
##############################

dflang1 <- dflang %>%
	#group_by(Sprache_Vortrag, atelier_sonstige) %>%
	group_by(Sprache_Vortrag) %>%
	summarise(Anzahl = n()) %>%
	ungroup() %>%
	mutate(relanz_prozent = Anzahl / sum(Anzahl) * 100)

p1 <- ggplot(dflang1) +
	geom_col(aes(x=Sprache_Vortrag, y=Anzahl, group=Sprache_Vortrag, fill=Sprache_Vortrag), 
					 colour="black", position="stack") +
	geom_text(aes(x=Sprache_Vortrag, y=-1, label=Anzahl), cex=10, hjust=0.5) +
	scale_fill_brewer(palette="Set2",
										guide=guide_legend(reverse=FALSE)) +
	#facet_wrap("atelier_sonstige") +
	#coord_flip() +
	theme_sst21() +
	labs(title="SST 2021, gesprochene Sprachen der Vorträge, absolut",
	     y = 'Anzahl Vorträge/Grussworte/Keynotes',
			 x = 'Sprache'
			 ) +
	theme(legend.position="none")

png(filename=paste(figdirprefix, filedateprefix, "_sprache-vortrag-absolut.png", sep=''),
		width=900, height=600)
 print(p1)
dev.off()

##############################
# Auswertung Dauer Vorträge pro Sprache
##############################

dflang2 <- dflang %>%
				group_by(Sprache_Vortrag) %>%
				summarise(
									dauer_gesamt = sum(Dauer_real_min),
								  dauer_plan = sum(Dauer_plan_min),
									dauer_gesamt_h = paste(round(dauer_gesamt/60, digits=1), "h", sep=""),
									anz = n()) %>%
				mutate(
							 delta_abs_min = dauer_gesamt - dauer_plan,
							 delta_relativ_prozent = round(dauer_gesamt / dauer_plan-1, digits=3)*100
							 )

# dflang2 <- dflang %>%
# 	#group_by(Sprache_Vortrag, atelier_sonstige) %>%
# 	group_by(Sprache_Vortrag) %>%
# 	summarise(Anzahl = n()) %>%
# 	ungroup() %>%
# 	mutate(relanz_prozent = Anzahl / sum(Anzahl) * 100)

p1 <- ggplot(dflang2) +
	geom_col(aes(x=Sprache_Vortrag, y=dauer_gesamt, group=Sprache_Vortrag, fill=Sprache_Vortrag), 
					 colour="black", position="stack") +
	geom_text(aes(x=Sprache_Vortrag, y=-20, label=dauer_gesamt_h), cex=10, hjust=0.5) +
	scale_fill_brewer(palette="Set2",
										guide=guide_legend(reverse=FALSE)) +
	#facet_wrap("atelier_sonstige") +
	#coord_flip() +
	theme_sst21() +
	labs(title="SST 2021, gesprochene Sprachen der Vorträge, Dauer",
	     y = 'Zeit [min]',
			 x = 'Sprache'
			 ) +
	theme(legend.position="none")

png(filename=paste(figdirprefix, filedateprefix, "_sprache-vortrag-dauer-summe.png", sep=''),
		width=900, height=600)
 print(p1)
dev.off()

p1 <- ggplot(dflang2) +
	geom_col(aes(x=Sprache_Vortrag, y=delta_relativ_prozent, group=Sprache_Vortrag, fill=Sprache_Vortrag), 
					 colour="black", position="stack") +
	geom_text(aes(x=Sprache_Vortrag, y=-2, label=delta_relativ_prozent), cex=10, hjust=0.5) +
	scale_fill_brewer(palette="Set2",
										guide=guide_legend(reverse=FALSE)) +
	#facet_wrap("atelier_sonstige") +
	#coord_flip() +
	theme_sst21() +
	labs(title="SST 2021, Zeitüberschreitung in %",
	     y = 'Delta t [%]',
			 x = 'Sprache'
			 ) +
	theme(legend.position="none")

png(filename=paste(figdirprefix, filedateprefix, "_sprache-vortrag-delta-prozent.png", sep=''),
		width=900, height=600)
 print(p1)
dev.off()


##############################

dflang2 <- dflang %>%
	group_by(Sprache_Vortrag) %>%
	summarise(Anzahl = n()) %>%
	ungroup() %>%
	mutate(relanz_prozent = round(Anzahl / sum(Anzahl),digits=2) * 100)

p2 <- ggplot(dflang2) +
	geom_col(aes(x="Sprache", y=relanz_prozent, 
							 group=Sprache_Vortrag, fill=Sprache_Vortrag), 
					 		colour = "black", position="stack") +
	geom_text(aes(x="Sprache", y=relanz_prozent, label=relanz_prozent, group=Sprache_Vortrag), 
						cex=8, position=position_stack(vjust=0.5)) +
	scale_fill_brewer(palette="Set2") +
	coord_flip() +
	theme_sst21() +
	theme(axis.text.y=element_text(angle=90, hjust=0.5)) +
	labs(title="SST 2021: gesprochene Sprachen der Vorträge in %",
	     #y = 'Anteil der Sprache in Prozent',
	     y = '',
			 x = ''
			 )

png(filename=paste(figdirprefix, filedateprefix, "_sprache-vortrag-relativ.png", sep=''),
		width=900, height=200)
 print(p2)
dev.off()

##############################

dflang3 <- dflang %>%
	mutate(lang_identisch = sprache_vortrag_folien_identisch) %>%
	group_by(lang_identisch) %>%
	summarise(Anzahl = n()) %>%
	ungroup() %>%
	mutate(relanz_prozent = round(Anzahl / sum(Anzahl),digits=3) * 100)

p3 <- ggplot(dflang3) +
	geom_col(aes(x="Anteil", y=relanz_prozent, 
							 group=lang_identisch, 
							 fill=lang_identisch), color="black", position="stack") +
	geom_text(aes(x="Anteil", y=relanz_prozent, 
								label=paste(relanz_prozent, "%", sep=""), 
								group=lang_identisch), 
						cex=7, position=position_stack(vjust=0.5)) +
	scale_fill_brewer(palette="Set2", direction=-1) +
	coord_flip() +
	theme_sst21() +
	theme(axis.text.y=element_text(angle=90, hjust=0.5)) +
	labs(title="SST 2021: Sprache Folien / Vortrag identisch?",
	     #y = 'Anteil TRUE/FALSE',
	     y = '',
			 x = ''
			 )

png(filename=paste(figdirprefix, filedateprefix, "_sprache-vortrag-identisch.png", sep=''),
		width=900, height=200)
 print(p3)
dev.off()

###############################
# Korrelationskoeffizienten zwischen Anzahl Autoren und Vortragsdauer
###############################

dfcor <- dflang %>%
				filter(!(Sprache_Vortrag %in% ("IT"))) %>%
				group_by(Sprache_Vortrag) %>%
				summarise(cor_autoren_dauer = cor(Autoren,Dauer_real_min))

p4 <- ggplot(dfcor) +
	geom_col(aes(x=Sprache_Vortrag, y=cor_autoren_dauer, fill=Sprache_Vortrag), color="black", position="stack") +
	scale_fill_brewer(palette="Set2", direction=1, guide=guide_legend(reverse=TRUE)) +
	coord_flip() +
	theme_sst21() +
	theme(
				axis.text.y=element_text(angle=0, hjust=0.5)) +
	labs(title="SST 2021: Korrelation zw. Anzahl Autoren und Dauer?",
	     x = '',
			 y = 'Korrelationskoeffizient'
			 )

png(filename=paste(figdirprefix, filedateprefix, "_korrelation-vortragssprache-dauer.png", sep=''),
		width=900, height=200)
 print(p4)
dev.off()

########
# Vorhersagemodell bauen
########
#
# Predictors: Autorenanzahl, Vortragstyp, Sprache von Titel und Folien
#
# Target: Vortragssprache
#
########

data_train <- dflang %>% filter(Tag %in% c(1,2))
data_test <- dflang %>% filter(Tag %in% c(3) )

formula <- Sprache_Vortrag ~ Autoren + Vortragstyp + Sprache_Titel + Sprache_Folien
logit <- glm(formula, data = data_train, family = 'binomial')
summary(logit)

predict <- predict(logit, data_test, type = 'response')
# confusion matrix
table_mat <- table(data_test$Sprache_Vortrag, predict > 0.5)
print(table_mat)
