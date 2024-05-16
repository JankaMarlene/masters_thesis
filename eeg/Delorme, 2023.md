# EEG is better left alone by Delorme, 2023
## Introduction
- EEG allows collection of large quantities of data, and automated preprocessing methods are critically needed to process the large publicy available EEG databases *Delorme, A. et al. Tools for importing and evaluating BIDS-EEG formatted data. In 2021 10th International IEEE/EMBS Conference on Neural Engineering (NER) 210–213. https:// doi. org/ 10. 1109/ NER49 283. 2021. 94413 99 (2021)*
- EEG is noisy and often contaminated by artifacts from the environment or the participants
- Eye movements and face, jaw & neck muscle contractions create scalp electrical potentials about 10 times the amplitude of brain signals and need to be removed
  - One strategy to remove noise: repeated presentation of stimuli in event-related potential paradigms
  - Other strategy: Use digital signal processing to remove artifacts
  - Inter-rater agreement gold standard for EEG data rejection (difficult and time-consuming and imprecise)
 ## Preprocessing methods
 - High-pass filtering
   - Linear filter length increases as the cutoff frequency decreases, making it impractical to filter EEG at frequencies velow 0.5 Hz
   - Used 4th-order Butterworth filter to assess the optimal high-pass filter cutoff frequency *(ERPLAP package; see "Methods" section)*
