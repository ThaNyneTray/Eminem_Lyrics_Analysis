library(dplyr)
library(tidyr)
library(tidytext)
library(ggplot2)
library(geniusr)
library(stringr)
library(Rspotify)
library(readr)
library(forcats)
library(purrr)
library(broom)


##################################################################################
############## GET LYRICS ########################################################
##################################################################################

# #### specify authorization key ###################################################
# keys<-spotifyOAuth(app_id="EvolutionOfHipHop",
#                    client_id="fb596ca0990a475fa1203b3dec8370a9",
#                    client_secret="a94a49dc86cf42e087dd68a30178fd5c")
# 
# we get the album list from  Spotify - genius API lacks this feature
# id<-searchArtist("Eminem", keys)$id[1]
# albums<-getAlbums(artist_id=id, token=keys)
# albums<-unique(albums$name)
# albums<-albums[c(1,2,3,5,6,9,11,12,14,16)]


# track_list<-NULL
# for (i in 1:length(albums)){
#   tracks<-get_album_tracklist_search("Eminem", albums[i])
#   tracks$album<-albums[i]
#   track_list<-rbind(track_list, tracks)
# }
# track_list
# 
# # 
# # # save the track_list data to a csv
# write.csv(track_list, file="./data/track_list_data.csv", row.names=F)

## load the track_list data frame
# track_list<-read.csv("./data/track_list_data.csv", stringsAsFactors=F)


# get lyrics for the entire tracklist, and combine them into a data frame
# lyrics<-c()
# for (i in 1:nrow(track_list)){
#   album_lyric<-get_lyrics_url(track_list$song_lyrics_url[i]) %>%
#     mutate(album=track_list$album[i],
#            track_number=track_list$song_number[i])
#   lyrics<-rbind(lyrics, album_lyric)
# }

# save the original lyric data
# write.csv(lyrics, "./data/original_lyrics.csv", row.names=FALSE)

##################################################################################
############## DATA IMPORT & CLEANING ############################################
##################################################################################

# DATA CLEANING
# 1. first, we are analyzing Eminem lyrics, so we want Eminem's words
#    he is known to write a lot of the hooks as well, but we can't be sure
# 2. we also don't need that column anymore, and the song urls,
#    or the artist name, so we remove it
# 3. need line numbers for analysis later, so we add those, 

original_lyrics<-read.csv("./data/original_lyrics.csv")
section_artists<-unique(original_lyrics$section_artist)

View(section_artists)

# str detect since some verses are rapped/sang together with other artists
# The song AROSE has the wrong section artist names.
# because the verses are labelled in parts of the song, and no
# indication of artist name.
lyrics<-original_lyrics %>%
  filter(str_detect(section_artist, "Eminem")|
         section_artist=="Arose"|
         section_artist=="Castle Extended") %>%
  rename(lyric=line, track_n=track_number) %>%
  group_by(song_name) %>%
  mutate(line=cumsum(song_name==song_name)) %>%
  select(lyric, line, album, track_n, song_name)

# save data to csv file for future access
# write_csv(lyrics, path="./data/lyrics_by_lines.csv")
# 
# # load lyrics data
# lyrics<-read_csv(file="./data/lyrics_by_lines.csv")

lyrics


##################################################################################
############## DATA CONDITIONING #################################################
##################################################################################

# Explore contractions

# with_contractions<-lyrics %>%
#   unnest_tokens(word,lyric) %>%
#   filter(str_detect(word, ".*'.*"))
# 
# with_contractions<-unique(with_contractions$word)
# View(with_contractions)
# 
# with_contractions<-with_contractions[which(str_detect(with_contractions, "'s")==FALSE)]
# 
# # check which lyrics have ain't
# ain_t<-lyrics %>%
#   filter(str_detect(lyric, "ain't"))
# ain_t  
# 
# ain_t$lyric <-str_replace_all(ain_t$lyric, "ain't", "aint")
# 
# # check lyrics with y'all
# y_all<-lyrics %>%
#   filter(str_detect(lyric, "y'all"))
# y_all  
# 
# # check lyrics with 'all
# any_all<-lyrics %>%
#   filter(str_detect(lyric, "'all"))
# any_all  
# 
# # check lyrics with e'ry
# e_ry<-lyrics %>%
#   filter(str_detect(lyric, "e'ry"))
# e_ry  
# 
# # check lyrics with c'mere
# c_mere<-lyrics %>%
#   filter(str_detect(lyric, "c'mere"))
# c_mere  
# 
# # check lyrics with don'tchu
# don_tchu<-lyrics %>%
#   filter(str_detect(lyric, "'tchu"))
# don_tchu
# 
# # check lyrics with g'd
# g_d<-lyrics %>%
#   filter(str_detect(lyric, "g'd"))
# g_d
# 
# # check lyrics with 'da
# da<-lyrics %>%
#   filter(str_detect(lyric, "'da"))
# da
# 
# # check lyrics with a'ight
# a_ight<-lyrics %>%
#   filter(str_detect(lyric, "aight"))
# a_ight
# 
# # check lyrics with 'em
# em<-lyrics %>%
#   filter(str_detect(lyric, "'em"))
# em

#
# explore contractions at end of words

# end_contractions<-lyrics %>%
#   # unnest_tokens(word, lyric) %>%
#   filter(str_detect(lyric, "'\\s"))
# 
# end_contractions<-str_extract(end_contractions$lyric, "\\w+'\\s")
# end_contractions<-unique(end_contractions)
# View(end_contractions)
# # by far the most common one is the contraction at the end of gerunds - in' instead of ing
# # testing removing them
# end_contractions<-str_replace_all(end_contractions, "in'\\s", "ing")
# 
# View(end_contractions)
# 
# # explore wanna, gonna, finna
# nna_s<-lyrics %>%
#   unnest_tokens(word, lyric) %>%
#   filter(str_detect(word, "nna"))
# 
# nna_s<-unique(nna_s$word)
# View(nna_s)
# 
# fix_contractions<-function(dat){
#   # as in the article, this could be a possesive or is/has
#   dat<-str_replace_all(dat, "'s", "")
#   dat<-str_replace_all(dat, "'m", " am")
#   # this one could be had or would, but I decide to replace with would
#   # barring analysis of tense, which I don't intend to do, this probably has no effect
#   dat<-str_replace_all(dat, "'d", " would")
#   # special cases of the n't contraction - won't and can't
#   dat<-str_replace_all(dat, "can't", "cannot")
#   dat<-str_replace_all(dat, "won't", "will not")
#   dat<-str_replace_all(dat, "don'tchu", "don't you")
#   # ain't is a special case.
#   dat<-str_replace_all(dat, "ain't", "aint")
#   dat<-str_replace_all(dat, "n't", " not")
#   dat<-str_replace_all(dat, "'re", " are")
#   dat<-str_replace_all(dat, "'ve", " have")
#   dat<-str_replace_all(dat, "'ll", " will")
#   dat<-str_replace_all(dat, "y'all", "you all")
#   dat<-str_replace_all(dat, "e'ry", "every")
#   dat<-str_replace_all(dat, "'da", " would have")
#   dat<-str_replace_all(dat, "a'ight", "all right")
#   dat<-str_replace_all(dat, "prob'ly", "probably")
#   dat<-str_replace_all(dat, "'em", "them")
#   # gerund contractions
#   dat<-str_replace_all(dat, "in'\\s", "ing")
#   # finna, wanna, gonna
#   dat<-str_replace_all(dat, "gonna", "going to")
#   dat<-str_replace_all(dat, "finna", "going to")
#   dat<-str_replace_all(dat, "wanna", "want to")
#   dat
# }
# # 
# lyrics$lyric<-tolower(lyrics$lyric)
# lyrics$lyric<-fix_contractions(lyrics$lyric)
# # lyrics
# # save cleaned data to csv file for future access
# write_csv(lyrics, path="./data/lyrics_by_lines.csv")
# 
# # load lyrics data
lyrics<-read_csv(file="./data/lyrics_by_lines.csv")

lyrics

##################################################################################
############## DESCRIPTIVE STATS #################################################
##################################################################################
summary(lyrics)

# some songs have a length of 1 line. This is strange - most proably skits
line_count<-lyrics %>%
  group_by(album, song_name) %>%
  count() %>%
  ungroup() %>%
  arrange(n)

line_count

# remove the skits and check what's left
line_count <- line_count %>%
  filter(!str_detect(song_name, "Skit"),
         !str_detect(song_name, "skit"))

short_titles<-subset(line_count, n<15)$song_name

# the first 5 are Eminem/other people talking. We get rid of those
# Eminem's shortest verse, it seems, is 24

lyrics<-lyrics%>%
  filter(!str_detect(song_name, "Skit"),
         !str_detect(song_name, "skit"),
         !song_name %in% short_titles)

line_count<-lyrics %>%
  group_by(album, song_name) %>%
  count() %>%
  ungroup() %>%
  arrange(n)

summary(line_count)
sd(line_count)

# let's plot a histogram of line length
ggplot(line_count, aes(x=n)) +
  geom_histogram(fill="lightblue", color="black", bins=25)+
  theme_bw() + 
  ylab("frequency") + xlab("line count")

# corresponding density kernel estimate
ggplot(line_count, aes(x=n)) +
  geom_histogram(aes(y=..density..), bins=25, 
                 fill="lightblue", color="black")+
  geom_density(fill="red", alpha=0.2)+
  theme_bw() + 
  ylab("frequency") + xlab("line count")

# let's see what's up with per album line count
ggplot(line_count, aes(x=album, y=n)) +
  geom_violin(fill="lightblue", trim=FALSE,
              alpha=0.5, show.legend=FALSE)+
  geom_boxplot(width=0.25, fill="white")+
  xlab("") + ylab("line count")+
  theme_bw()
  

##################################################################################
############## WORD FREQUENCIES ##################################################
##################################################################################


# 1. here we use unnest tokens to get a column of words (tidy text) on its own, which 
#    streamlines the analysis process
# 2. we also remove stop words, using anti join

data("stop_words")

tidy_lyrics<-lyrics %>%
  unnest_tokens(word, lyric) %>%
  anti_join(stop_words) 

words_total<-tidy_lyrics %>%
  count(word) %>%
  arrange(-n)

words_total
# 3. In true Eminem fashion, we find some curses amongst his top 10 most used
# 4. Also notice that no 2 and 4 are practically the same. THis would be the 
#    perfect time to add some lemmatization

# words_ld<-tidy_lyrics %>%
#   mutate(lemma=textstem::lemmatize_words(word)) %>%
#   select(word, lemma) %>%
#   filter(word != lemma) %>%
#   distinct(word, lemma)

# 5. let;s see what difference it makes in total count
words_total_lemma<-tidy_lyrics %>%
  mutate(lemma=textstem::lemmatize_words(word)) %>%
  # group_by(lemma) %>%
  count(lemma) %>%
  arrange(-n)

# 6. THe lemmatization caught  some extra variations of some words,
#    However, it missed "fucking" because of the missing 'g' in its gerund
# 7. We can modify this manually. At the same time, yeah was corrected to yeah,
#    I don't like that. So we correct it.
# 8. First, store the unlemmatized lyrics, in case we need them later
lyrics_unlemma<-tidy_lyrics

tidy_lyrics<-tidy_lyrics %>%
  mutate(word=textstem::lemmatize_words(word),
         word=ifelse(word=="yes", "yeah", word),
         word=ifelse(word=="fuckin", "fuck", word))

# 9. Finally, we get the frequency, and split by album
# 10.TOtal frequency
tidy_lyrics %>%
  count(word) %>%
  arrange(-n)

# 11. Let's see the frequency difference across albums
# 12. It might be more reasonable to look at the proportions as
#     opposed to raw counts. The album lengths are bound to be different
# 13. Basically, which words appear in highest proportion in each album?
freq<-tidy_lyrics %>%
  count(album, word) %>%
  # arrange(-n) %>%
  group_by(album) %>%
  mutate(total=sum(n)) %>%
  # ungroup() %>%
  mutate(freq=n/total)

freq
# 14. To make it easier to see, let's plot it
#     TODO: come back to this. How to rearrange plot
freq %>%
  group_by(album) %>%
  # top_n(10, freq) %>%
  arrange(-freq) %>%
  # mutate(word=factor(word, freq)) %>%
  ggplot(aes(word, freq, fill=album)) +
  geom_col(show.legend = FALSE) +
  # facet_wrap(~album, ncol=3, scales="free") +
  coord_flip() + theme_bw()

# 15. let's see about word correlations,
#     We wanna spread the values of the album row to take individual columns.
freq<-freq %>%
  select(album, word, freq) %>%
  spread(album, freq, fill=0)

# 16. Here since we are using a bivariate plot, let's choose some albums to compare
#     WIth how terriby Revival was received, one of the mottos was 
#     "Bring the Real Slim Shady back". So let's see how it compares to 
#     Slim Shady LP
sslp_revival<-freq %>%
  select(word, `The Slim Shady LP`, Revival) %>%
  arrange(-Revival, -`The Slim Shady LP`) %>%
  filter(!Revival==0 & !`The Slim Shady LP`==0)

ggplot(sslp_revival, aes(`The Slim Shady LP`, Revival)) +
  geom_jitter(alpha=0.1, size=2.5, width=0.25, height=0.25)+
  geom_text(aes(label=word), check_overlap=TRUE, vjust=1.5) +
  scale_x_log10(labels=scales::percent_format())+
  scale_y_log10(labels=scales::percent_format())+
  geom_abline(color="red") +
  theme_bw()

# 17. Since this is something we might have to do repeatedly for different pairs of 
#     albums, let's wrap it in a function
albums_corplot<-function(album1, album2, freq){
  album1<-enquo(album1)
  album2<-enquo(album2)
  
  freq<-freq %>%
    select(word, !!album1, !!album2) %>%
    filter(!(!!album1==0) & !(!!album2==0))
  print(album1)
  print(album2)
  ggplot(freq, aes(!!album1, !!album2)) +
    geom_jitter(alpha=0.1, size=2.5, width=0.25, height=0.25)+
    geom_text(aes(label=word), check_overlap=TRUE, vjust=1.5) +
    scale_x_log10(labels=scales::percent_format())+
    scale_y_log10(labels=scales::percent_format())+
    geom_abline(color="red") +
    theme_bw()
}

albums_corplot(`The Slim Shady LP`, Revival, freq)
albums_corplot(`The Marshall Mathers LP`, `The Marshall Mathers LP2`, freq)
albums_corplot(Kamikaze, Revival, freq)

##################################################################################
############## COMPARING WORD USAGE ##############################################
##################################################################################

# 18. Here we wanna check which words are more likely to show up in which 
#     albums. We use log odds ratio
word_ratios<-tidy_lyrics %>%
  count(word, album) %>%
  group_by(word) %>%
  filter(sum(n)>10) %>%
  ungroup() %>%
  spread(album, n, fill=0) %>%
  mutate_if(is.numeric, list(~(. + 1)/(sum(.)+1))) %>%
  mutate(logratio=log(`The Slim Shady LP`/Revival)) %>%
  arrange(desc(logratio)) %>%
  select(word, `The Slim Shady LP`, Revival, logratio)
  
word_ratios %>%
  arrange(abs(logratio))

# 19. Let's plot the top 15 in these
word_ratios %>%
  group_by(logratio<0) %>%
  top_n(10, abs(logratio)) %>%
  ungroup() %>%
  mutate(word = reorder(word, logratio)) %>%
  ggplot(aes(word, logratio, fill=logratio<0)) +
  geom_col(show.legend = FALSE) +
  coord_flip() + theme_bw() +
  ylab("log odds ratio (The Slim Shady LP/Revival)") +
  scale_fill_discrete(name="", labels=c("The Slim Shady LP", "Revival"))
  
# 20. Wrap these in functions
get_log_odds<-function(dat, album1, album2){
  album1<-enquo(album1)
  album2<-enquo(album2)
  
  word_ratios<-dat %>%
    count(word, album) %>%
    group_by(word) %>%
    filter(sum(n)>10) %>%
    ungroup() %>%
    spread(album, n, fill=0) %>%
    mutate_if(is.numeric, list(~(. + 1)/(sum(.)+1))) %>%
    mutate(logratio=log(!!album1/!!album2)) %>%
    arrange(desc(logratio)) %>%
    select(word, !!album1, !!album2, logratio)
  
  word_ratios
}

word_ratios<-get_log_odds(tidy_lyrics, `The Marshall Mathers LP`, `The Marshall Mathers LP2`)

plot_log_odds<-function(word_ratios, album1, album2){
  # album1<-enquo(album1)
  # album2<-enquo(album2)
  
  word_ratios %>%
    group_by(logratio<0) %>%
    top_n(15, abs(logratio)) %>%
    ungroup() %>%
    mutate(word = reorder(word, logratio)) %>%
    ggplot(aes(word, logratio, fill=logratio<0)) +
    geom_col(show.legend = FALSE) +
    coord_flip() + theme_bw() +
    ylab(paste0("log odds ratio (", album1,"/", album2,")" )) +
    scale_fill_discrete(name="", labels=c(album1, album2))
}

plot_log_odds(word_ratios, "The Marshall Mathers LP", "The Marshall Mathers LP2")


##################################################################################
############## CHANGES IN WORD USE OVER TIME #####################################
##################################################################################

# 21. This is actually pretty exciting. How has his word usage evolved over time
# 22. First, we make a data frame with word count per album, and word count across 
#     all albums. I converted albums to integers for the model to work
# 22a IMPORTANT: makes sure to check that what I'm doing makes sense
# 22b I think what makes sense is to makes the albums dates instead

# do this with geniusr if possible
album_year<-c("01-17-2020","08-31-2018","12-15-2017","11-5-2013","06-18-2010",
             "05-15-2009","11-12-2004","05-26-2002","05-23-2000","02-23-1999")
origin<-as.Date("01-01-1995", format="%m-%d-%y")
album_year<-as.Date(album_year, format="%m-%d-%y", origin=origin)

album_dates<-tibble(album=albums,album_year=album_year)

words_by_album<-tidy_lyrics %>%
  inner_join(album_dates, by=c("album")) %>%
  count(album_year, album, word) %>%
  rename(count=n) %>%
  group_by(album) %>%
  mutate(album_total=sum(count)) %>%
  group_by(word) %>%
  mutate(word_total=sum(count)) %>%
  ungroup() %>%
  # mutate(album=factor(album, albums),
  #        album=as.numeric(album)) %>%
  filter(word_total > 100) %>%
  arrange(-album_total) 

words_by_album
# albums
# 23. Each row corresponds to the total number of times a word is used within an album
#     as well as total number it's used across all albums
# 24. Used nest to create list columns with miniature data frames for each word. WHY??

nested_albums<-words_by_album %>%
  mutate(album_year=as.numeric(album_year)) %>%
  nest(data=c(album_year, count, album_total, word_total))

nested_albums

# 25. Import purrr, to use the map functionality
nested_models<-nested_albums %>%
  mutate(models = map(data, ~glm(cbind(count, album_total)~album_year, ., 
                                 family="binomial")))

nested_models$models

# 26. We get a model for each of the words in the nested_albums data frame
#     We then use map() and broom() to extract the stastistically significant
#     ones, and adjust for multiple comparisons. Import broom for this

slopes<-nested_models %>%
  mutate(models=map(models, tidy)) %>%
  unnest(cols=c(models)) %>%
  mutate(adjusted.p.value=p.adjust(p.value)) 

top_slopes<-slopes %>%
  filter(adjusted.p.value<0.05)

top_slopes

words_by_album %>%
  inner_join(top_slopes, by=c("word")) %>%
  ggplot(aes(album, count/album_total, color=word)) +
  geom_line(size=1.3) +
  labs(x=NULL, y="word frequency")+
  theme_bw()

  
##################################################################################
############## SENTIMENT ANALYSIS ################################################
##################################################################################




##################################################################################
############## TOPIC MODELING ####################################################
##################################################################################

