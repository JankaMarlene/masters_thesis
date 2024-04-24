# Cluster Analysis. Everitt et al., 2011
hhkhj
## Introduction
- Classification
  - For organizing large data set, so it can be understood more easily & information retrieved more efficiently
  - If data validly summarized by small number of groups of objects, then the group labels may provide very concise description of patterns of similarities and differences in data
  - To understand & treat disease it has to be classified
    - Two main aims
      - **prediction**
      - **aetiology**
        - Causes of different types of disease
- *Object* -> people we look at
- Overlapping clusters
- Cluster analysis essentially about discovering groups in data
- Cluster define it in terms of internal cohesion **homogeneity** & external isolation **separation**
- Cluster analysis techniques are concerned with exploring data sets to assess whether or not they can be summarized meaningfully in terms of a relatively small number of groups or clusters of objects or  individuals which resemble each other and which are different in some respects from individuals in other clusters

## Model-based cluster analysis for structured data
- HADS -> Multivariate datas (anxiety and depression) meassured on the same scale
- Described here: More formal statistical alternative to the two-stage approach
- Structured data

## Some final comments and guidelines
- Cluster analysis does not involve simply the application of particular techenique bur rather necessitates a series of steps, each of which may be dependent on results of the preceding one
- It is generally impossible **a priori** to anticipate what combination of variables, similarity measures and clustering techniques is likely to lead to interesting and informative classifications
- Analysis proceeds through several stages
- Final, extremely important stage concerns the evaluation of the clustering solutions obtained
  - Are the clusters real or merely artefacts of the algorithms?
  - Do other solutions exist which are better?
  - Can the clusters be given a convincing interpretation?
  - A long list of such questions (which are full of traps for the unwary; see *Dubes and Jain, 1979*) might be posed
- No one clustering method can be judged to be "best" in all circumstances
- Particular methods will be best for particular types of data
- Many applications reasonable to apply number of clustering methos
  - All produce similar solutions, the investigator might, justifiably perhaps, have more confidence that results are worthy of further investigation
  - Widely different solutions might be taken as evidence against any clear-cut cluster structure
- Packages overview *Page 260*

### Steps in typical cluster analysis suggested by *Milligan (1996)*
1. Objects to cluster
   - Representative of cluster structure believed to be present
   - Randomly sampled if generalization to a larger population is required
   - Over-sampling of small populations and placement in sample of "ideal types" may be acceptable so long as generalization is not required
     - Cluster analysis is not an inferential technique
2. Variables to be used
   - Only included if good reasin to think they will define the clusters
   - Irrelevant or **masking** variables should be excluded if possible
3. Missing values
4. Variable standardization
   - Not necessarily always indicated, sometimes misleading (*Section 3.8*)
   - Simulations of Miligan and Cooper (1988)
   - Another solution: Of choosing appropriate unit of measurement is to emply a cluster method that is invariant under scaling -> (*Chapeter 6*)
5. Proximitymeasure
   - Few general guidlines for this (*Section 3.9*) (*Chapter 3*)
6. Clustering method
   - Designed to recover the types of clusters suspected
   - Effective at recovering them
   - Insensitive to error
   - Available in software
7. Number of clusters
8. Replication and testing
   - **Cross-validation techniques** -> how far clusters indentified in a subsample are still identifiable among the subsample of objects not used in clustering
   - **Perturbation of the sample** by omitting or slightly changing particular data points
   - *section 9.5* techniques for assessing internal validity
   - Goodness-of-fit statistics calculated to compare clusters to the data used to derive them
   - Quality assessment involve comparing results between substes, between the sample and a second sample or an external classification, using, f.eks. Rand index for comparing partitions, or the cophenetic correlation for comparing dendrograms (*Section 9.4*)
9. Interpretation
   - Graphical representation (*Section 9.6*)
   - Descriptive statistics
   - Standard statistical tests such as analysis of variance are inappropriate for comparing clustering variables between clusters, since the clustering technique will have maximized between-cluster differences on these variables in some way
### Testing for absence of structure
- Verstehe noch nicht ganz, ob ich das brauche oder nicht
- Not necessary, if reason for clustering is practical (e.g. for organizational purpose)
### Methods for comparing cluster solutions
- Comparing partitions
- Comparing dendrograms
- Comparing proximity matrices
  
### Internal cluster quality, influence and robustness
- **Internal cluster quality** taken to refer to extent to which cluster meet the requirements for good clusters, as defined by *Cormack (1971)*, namely isolation and cohesion
  - *9.5.1*
- **Robustness** refers to effects of errors in data or missing observations, and changes in the data or methods
  - *9.5.2*
- ***9.5.3*** **Influence of individual points**

### Displaying cluster solutions graphically
- Before applying any clustering method, some graphical representation of the data should be obtained
- *Chapter 2*

### Illustrative examples
- further illustrative examples given
- 2 applications: How data matrices, priximity matrices & alternative hierarchical clusterings can be compared
- 1 application: Using model-based technique, which illustrates how number of steps can be applied to reduce the complexity of data

### Summary
- Valuable tools in exploration of multivariate data
- Organizing such data into subgroups or clusters, clustering may help investigator discover characteristics of any structure or pattern present
