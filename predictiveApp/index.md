---
title       : Predict App
subtitle    : Data Science Capstone, August 2015
author      : ariellev02@gmail.com
framework   : io2012      
highlighter : highlight.js  
widgets    : [bootstrap, quiz, shiny, interactive]
mode        : selfcontained 
knit        : slidify::knit2slides
--- 

<q>Those who have knowledge, don't predict. Those who predict, don't have knowledge.</q>
<q>Lao Tzu</q>
<q style="font-size:20pt; margin-top:5px">6th Century BC Chinese Poet</q>

---

Overview
---
<br/>
* PredictApp is a predictive text application.
* The language model was developed on a sample of 15 thousand (0.3%) sentences taken<br/>from HC Corpora corpus. The corpus was assembled by crawling twitter,<br/>blog and news websites. More info [HC Corpora](http://www.corpora.heliohost.org)
* I removed profanity and non-alphabetic tokens. 
* I trained unigram, bigram, trigram and 4-gram by assigning<br/>maximum-likelihood probabilities.
* Model produced 532.5k unigrams and 24.9k word types.
* https://ariellev.shinyapps.io/predictApp
* https://github.com/ariellev/predictApp

---

Usage
---
<br/>
* Very intuitive to use. PredictApp is much like any other familiar predicitve text software<br/>found in nowadays mobile devices.
* User types-in, app comes up with suggestions along the way. 
* User can anytime append a suggestion to the input line by clicking over it.
* Suggestions are sorted from most (left) to least (right) likeable.
* User can post, clear or reset the application.

---

## Screenshot

<!-- Limit image width and height -->
<style type="text/css">
img {     
  max-height: 560px;     
  max-width: 964px; 
}
</style>
<img src="predictiveApp.png"/>
<!-- Center image on slide -->
<script type="text/javascript" src="http://ajax.aspnetcdn.com/ajax/jQuery/jquery-1.7.min.js"></script>
<script type="text/javascript">
$(function() {     
  $("p:has(img)").addClass('centered'); 
});
</script>
