## Predicting International Student Enrollment by Institutional Aid: Using Fixed and Random Effects

This repository contains replication code and a summary. 
Suggested citation: Posmik, Daniel (2022). "Predicting International Student Enrollment by Institutional Aid: A Random and Fixed Effects Approach",_Journal of Student Financial Aid, Vol. 51, Iss. 3 , Article 4_.

* `analysis.R` main file

* `dta.csv` data file

**Paper Abstract:** Since the fall semester of 2016, first-time international student enrollment (ISEft) has declined at U.S. colleges and universities. This trend disrupts a steady upwards trajectory of ISEft rates. Previous research has demonstrated that various political, social, and macroeconomic factors influence the amount of international students studying in the U.S. Exploiting data from the Common Data Set (CDS), I focus on the role financial aid plays as an enrollment predictor for international undergraduate students. A fixed effects model reveals that financial aid is strongly and significantly predictive of ISEft, yielding a 1.8% enrollment increase per 10% aid increase, all else equal. Interestingly, financial aid is only predictive of ISEft if it is awarded in substantial amounts. Extending the work of Bicak and Taylor (2020), I also analyze how the effectiveness of financial aid awards varies within different institutional settings. Random effects regressions reveal that rural, low research, and private universities experience considerable marginal ISEft boosts when awarding aid to international students. The findings of this work are primarily directed at institutional leaders who seek to revitalize their institution’s ISEft policy. Moreover, these insights may inform local policymakers who seek to incent ISEft.

**Keywords:** _International Student Enrollment, Financial Aid, Panel Data Regression, Treatment Effect Heterogeneity_

### Presentation Links
- Presentation at **[2022 Cleveland Fed Economic Scholars Program](/pdf/PosmikDaniel_ESPPresentation.pdf)**
- Guest Lecture for Dr. Asawari Deshmukh's **[International Economics (ECON4040)](/pdf/Posmik_GuestLecture_ECON4040.pdf)** course

### 1. The relationship of International Student Enrollment and Financial Aid

Financial aid is an important subsidy for most international students financing their education in the U.S. But to what extent does financial aid predict the enrollment of first-time international students (ISEft)? To answer this question, I leverage international student aid data from the Common Data Set (CDS). CDS data is the only available source of international student aid data, aside from government data. Notably, CDS data is  - somewhat counterintuitively to its name - not centrally aggregated. I spent the better half of an academic year scraping data from individual institution's websites. Therefore, time constraints necessitated narrowing down the paper's scope to institutions in the Great Lakes region (OH, MI, WI, IN, IL). 

Interestingly, previous literature offers a wealth of insight into the relationship of financial aid and enrollment. The literature, however, only (i) considers certain institutions (e.g., Zhang and Hagedorn, 2018; Bound et al., 2020), (ii) is limited to domestic students (e.g., Dynarski, 2000, 2003; Kane, 2010; Leslie and Brinkman, 1987; Seftor and Turner, 2002), or (iii) is restricted to time-invariant ISEft predictors (e.g., Bicak and Taylor, 2020). My paper combines various insight from literature with CDS data in an attempt to understand the role of financial aid as an enrollment predictor for international undergraduate students. 

### 2. Causality

_Causal inference_ or _Casual inference_ - that is the question. Initially, my paper gave rise to a difference-in-differences scenario (DiD) with continuous and multiple-period treatment (henceforth the "cont.-mp" case). Callaway et al. (2021) - the theoretical basis for this paper's causal inference design - outline five criteria that would allow for a causal interpretation in the cont.-mp scenario:

- Assumption 1 (Random Sampling): The observed data are independent and identically distributed (iid).
- Assumption 2 (Support): There is a control group of units that are general enough to allow for continuous treatment.
- Assumption 3 (No Anticipation / Staggered Adoption): Units do (i) not anticipate treatment and (ii) upon becoming treated with dose d at time t, remain treated with dose d in all subsequent periods.
- Assumption 4 (Strong Parallel Trends): Restricts paths of both untreated and treated potential outcomes such that that all dose groups treated at time t would have had the same path of potential outcomes at every dose.
- Assumption 5 (No Treatment Effect Dynamics/ No Treatment Effect Heterogeneity): Homogeneous behavior of treated units across groups, treatment time, and treatment timing group

Assumptions 1 - 3 are _weaker_ assumptions that can be justified even with observational data. Assumptions 4 and 5, however, are much stronger ones. Specifically, Assumption 5 is where the problem lies in my analysis. In my paper, I consider how changes in total aid predict changes in aggregate ISEft at time t at institution i. In order to rule out Treatment Effect Heterogeneity (TEH), I would have to assume that individual characteristics, such as family income or ability (Dynarski, 2000; Cornwell et al., 2006) are quasi-random across institutions i and time periods t. More generally, I would have to assume homogeneity of the causal response (think: _Effect of aid on an individual student_) across incoming student classes across all institutions and all time periods. That is simply not the case, and any attempt to spin it that way would have been a poor attempt at embellishing the quality of my data. So, in order to avoid authoring yet another _casual inference_ paper, I decided to reject a causal framework altogether and focus on the value this study delivers as a correlational study. Mainly, it is a valuable tool for institutional decisionmakers wondering about "how far" their aid money can go. (Sneak peek into the next section: Pretty far!)  

### 3. Modeling

The general model controls for three variables. The log of cost (since aid is only important when relative to cost (see Bodycott, 2009; Darby, 2015)); the perceived quality of the institution via acceptance rate (see Bodycott, 2009; Darby, 2015; Mazzarol and Soutar, 2002), and finally institutional size via the log of total undergraduate enrollment (see Cantwell, 2019). The variables of interest are the log of financial aid as well as aid concentration - a measure of whether _Many students receive little aid_ or _few students receive a lot of aid_. The dependent variable is the log of ISEft. 

Statistical testing as well as literature led to choose a 2-way fixed effects (TWFE) model. 

<img src="images/models.jpg?raw=true"/>

The results show that both financial aid and aid concentration are the most significant predictors of ISEft. All else equal, a 10% increase in financial aid yields a 1.8% increase in ISEft. Done!

... wait ...

Didn't I forget about something? Wasn't there something about treatment effect heterogeneity? Yes! While it is still impossible to recover causal parameters, I can dig deeper into the heterogeneous nature of aid awards in a predictive framework. 

The second part of the paper uses a random effects (RE) model to examine whether aid predictions vary when conditioned on certain institutional characteristics. I examine the interaction of financial aid within the most important (see Bicak and Taylor, 2020) time-invariant institutional predictors of ISEft: Location (Rural, Suburban, Urban), Sector (Private, Public), and Research Intensity (Low, Medium, High). In a nutshell, those are highly significant ISEft predictors that institutions cannot change (easily). Using a RE model, I interact the log of financial aid variable with a each of those time-invariant characteristics. The coefficient on this interaction term tells us _how much more_ (→ positive coefficient on interaction term) or _how much less_ (→ negative coefficient on interaction term) powerful aid is within this specific characteristic. 

<img src="images/results2.jpg?raw=true"/>

The results are fascinating! We can see that rural, low research intensity, and private institutions experience enrollment boosts when compared to their counterparts (note how important a precise interpretation of this RE model is). This suggests that the institutions that generally have the lowest average international student enrollment can profit the most by awarding aid. Especially rural institutions have a high enrollment boost of 4.5% when compared to their suburban and urban counterparts.  

### 4. Results and Implications

These findings - especially the latter ones - have important consequences for institutions. Not only is aid a win-win tool (e.g., Mause, 2009) for international students alike, rural, private, low research institutions can leverage financial aid at a greater efficiency than their counterparts. Since international students bring funding and funding for institutions (see McCormack, 2007; Chellaraj et al., 2008), these institutions use aid to gain an edge in the higher education industry. In competition, rural, private, low research universities can pursue an internationalization strategy to compete with structurally more desirable institutions (e.g., urban, public, high research). 

Lastly, this insight may also be of value for local policymakers. Aid is an effective and reliable tool to attract and retain international talent for policymakers (Douglass and Edelstein, 2009; Li, 2017), creating a means to affect economic development/ growth in certain regions/ industries via universities as intermediaries (Owens et al., 2011). All in all, I conclude that financial aid is a reliable enrollment management tool that prioritizes students, institutions, and economic interests.

