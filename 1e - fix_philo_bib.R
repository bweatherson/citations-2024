# Fix philo_bib

require(tidyverse)

load("philo_bib_unlisted.RData")
load("philo_cite.RData")

authadjust <- function(x){
  paste0(str_extract(x, '\\b[^,]+$'), " ", str_to_title(str_extract(x,".+(?=,)")))
}

authadjust_short <- function(x){
  str_to_title(str_extract(x,".+(?=,)"))
}

# philo_bib_fix_1 <- philo_bib_fix_1 |>
#   rowwise() |>
#   mutate(auth = case_when(
#     length(author) == 1 ~ authadjust(author[1]),
#     length(author) == 2 ~ paste0(
#       authadjust(author[1]), 
#       " and ", 
#       authadjust(author[2])
#       ),
#     length(author) == 3 ~ paste0(
#       authadjust(author[1]), 
#       ", ", 
#       authadjust(author[2]), 
#       ", and ",
#       authadjust(author[3])
#     ),
#     length(author) == 4 ~ paste0(
#       authadjust(author[1]), 
#       ", ", 
#       authadjust(author[2]), 
#       ", ",
#       authadjust(author[3]),
#       ", and ",
#       authadjust(author[4])
#     ),
#     TRUE ~ paste0(authadjust(author[1]), " et al")
#   )) |>
#   ungroup()




name_fix <- c(
  'Bonjour' = 'BonJour',
  'Mcdowell' = 'McDowell',
  'Mcmullin' = 'McMullin',
  'Mcgee' = 'McGee',
  'Wedgewood' = 'Wedgwood',
  'Mcginn' = 'McGinn',
  'Mcgrath' = 'McGrath',
  'Mckinsey' = 'McKinsey',
  'Macfarlane' = 'MacFarlane',
  'Castaneda' = 'Castañeda',
  'Hajek' = 'Hájek',
  'Vaninwagen' = 'van Inwagen',
  'Vanfraassen' = 'van Fraassen',
  'D\'arms' = 'D\'Arms',
  'Godfreysmith' = 'Godfrey-Smith',
  'Vangelder' = 'van Gelder',
  'Derosset' = 'deRosset',
  'Mccloskey' = 'McCloskey',
  'Derose' = 'DeRose',
  'Mchugh' = 'McHugh',
  'Ronnow‐Rasmussen' = 'Rønnow‐Rasmussen'
)

philo_bib_fix_2 <- philo_bib_fix_1 |>
  mutate(graph_auth = str_replace_all(
    graph_auth,
    name_fix
  )) |>
  mutate(short_auth = str_replace_all(
    short_auth,
    name_fix
  )) |>
  mutate(auth = str_replace_all(
    auth,
    name_fix
  )) 

philo_bib_fix_3 <- philo_bib_fix_2 |>
  ungroup() |>
  mutate(journal = str_replace_all(journal, 'Nous', 'Noûs')) |>
  mutate(art_title = str_replace_all(
    art_title,
    c(
      ' A ' = ' a ',
      ' And ' = ' and ',
      ' The ' = ' the ',
      ' Or ' = ' or ',
      ' As ' = ' as ',
      ' Of ' = ' of ',
      ' On ' = ' on ',
      ' In ' = ' in ',
      ' For ' = ' for ',
      ' Is ' = ' is ',
      'Decision-Theory' = 'Decision Theory',
      'Modern-History' = 'Modern History',
      'Times Arrow' = 'Time\'s Arrow',
      'Derosset' = 'deRosset',
      'Macfarlane' = 'MacFarlane',
      'Norms Of Assertion (Knowledge Norm Of Assertion, Kna)' = 'Norms of Assertion',
      'Derose' = 'DeRose',
      'Mcgrath' = 'McGrath',
      'Trust? (Epistemic Considerations)' = 'Trust?',
      'D\'arms' = 'D\'Arms',
      'Dimensionalism (Persistence Through Time, Doctrine Of Temporal Parts, Perdurance)' = 'Dimensionalism',
      'Frankfurt Attack on the Principle-Of-Alternative-Possibilities' = 'Frankfurt\'s Attack on the Principle of Alternative Possibilities',
      'Chance And Credence - Humean Supervenience Debugged' = 'Humean Supervenience Debugged',
      'Mckinsey' = 'McKinsey',
      '2 Modelings ' = 'Two Modelings ',
      'Moral Realism + A Form Of Ethical Naturalism' = 'Moral Realism',
      'Actualism + The Exploration And Defense Of Actualism' = 'Actualism',
      'Mcgee' = 'McGee',
      'Vanfraassen' = 'van Fraassen',
      'Putnam Paradox' = 'Putnam\'s Paradox',
      '2 Distinctions' = 'Two Distinctions',
      '3 Theses' = 'Three Theses',
      '2 Notions' = 'Two Notions',
      'Godfreysmith' = 'Godfrey-Smith',
      '2 Concepts' = 'Two Concepts',
      '2 Kinds' = 'Two Kinds',
      '2 Types' = 'Two Types',
      '1 Object' = 'One Object',
      'Mcdowell' = 'McDowell',
      '1st' = 'First',
      'G.e. Moore' = 'G.E. Moore',
      'To-Be' = 'To Be',
      'De-Dicto' = 'De Dicto',
      'De-Se' = 'De Se',
      'Problem of Action' = 'The Problem of Action',
      'Matter of Individuality' = 'A Matter of Individuality',
      'Logic of Paradox' = 'The Logic of Paradox',
      'Problem of the Essential' = 'The Problem of the Essential',
      'and a Ariew' = 'and A Ariew',
      'Ceteris-Paribus' = 'Ceteris Paribus'
    )
  )) |>
  mutate(art_title = str_replace_all(
    art_title,
    fixed(
      c(
        'Have a Foundation' = 'Have a Foundation?',
        'What An Antiindividualist Knows A-Priori' = 'What An Anti-Individualist Knows A Priori',
        'Functional-Analysis and Proper Functions' = 'Functional Analysis and Proper Functions',
        'to Modus-Ponens' = 'to Modus Ponens',
        'Easy Possibilities (Reliability-Conditional)' = 'Easy Possibilities',
        'Four-Dimensionalism' = 'Four Dimensionalism',
        'The Conceptual Analysts Defense' = 'The Conceptual Analyst\'s Defense',
        'Materialism and Qualia, the Explanatory Gap' = 'Materialism and Qualia: The Explanatory Gap',
        'Saints + Implications for Moral-Philosophy' = 'Saints',
        'All the Worlds a Stage + a Philosophers Reasons To Believe the Stage-View' = 'All the World\'s a Stage',
        'Stalking the Wild Epistemic Engine + Computational Psychology and Cognition' = 'Stalking The Wild Epistemic Engine',
        'Structures of Normative Theories + Addressing the Agent-Neutral and Agent-Centered Distinctions as Substantive Mistakes in Moral Theorizing' = 'Structures of Normative Theories',
        'Moral Realism + a Form of Ethical Naturalism' = 'Moral Realism',
        'Oughts, Options, and Actualism + the Exploration and Defense of Actualism' = 'Oughts, Options, and Actualism',
        'Sense, Nonsense and the Senses, An Inquiry Into the Powers of the Human Mind + 1994 Dewey Lectures At Columbia-University, Lecture 1 - the Antinomy of Reason' = 'Sense, Nonsense, and the Senses: An Inquiry into the Powers of the Human Mind',
        'Finkish Dispositions + Refutation of Simple Conditional Analysis' = 'Finkish Dispositions',
        'Four-Dimensionalism (Persistence Through Time, Doctrine of Temporal Parts, Perdurance)' = 'Four-Dimensionalism',
        'The Extended Mind (Active Externalism)' = 'The Extended Mind',
        'Brutal Composition (Objects, Vagueness, Ontology)' = 'Brutal Composition',
        'Consciousness The Transmutation Of A Concept' = 'Consciousness: The Transmutation Of A Concept',
        'Dispositions and Antidotes (Reply To David Lewis)' = 'Dispositions and Antidotes',
        'Nonexistence (Singular Negative Existentials)' = 'Nonexistence',
        'Is Coherence Truth Conducive? (Belief)' = 'Is Coherence Truth Conducive?',
        'Is There A Fundamental Level? (Reality, Metaphysics, Ontology)' = 'Is There A Fundamental Level?',
        'Emotions (Ethics, Propriety, Correctness)' = 'Emotions',
        'Antidotes (Reply To David Lewis)' = 'Antidotes',
        'Mind (Active Externalism)' = 'Mind',
        'Clay (Problem in Philosophy, Ontology)' = 'Clay',
        ' (Properties, Philosophy)' = '',
        ' (Rationality, Correct Reasoning)' = '',
        ' (A Philosophical Consideration on Propositional Definitions of Acquired Knowing)' = '',
        ' (Epistemic Considerations)' = '',
        'G.e. Moore And John Mcdowell' = 'G.E. Moore and John McDowell',
        ' (Epistemic Warrant)' = '',
        ' (Reality, Metaphysics, Ontology)' = '',
        ' (Knowledge Norm of Assertion, Kna)' = '',
        ' (Keith Lehrer)' = '',
        'Best Explanation - Criteria for Theory Choice' = 'The Best Explanation: Criteria for Theory Choice',
        'What is Equality .1. Equality of Welfare' = 'What is Equality? Part 1: Equality of Welfare',
        'What is Equality .2. Equality of Resources' = 'What is Equality? Part 2: Equality of Resources',
        'Fallacy: on' = 'Fallacy: On',
        'Chance and Credence - Humean Supervenience Debugged' = 'Humean Supervenience Debugged',
        'The Rule-Following Considerations .5.' = 'The Rule-Following Considerations',
        'Aesthetic Attitude .5.' = 'Aesthetic Attitude',
        'A Causal Calculus .1.' = 'A Causal Calculus (I)',
        'A Causal Calculus (Ii)' = 'A Causal Calculus (II)',
        'Why Did Einsteins Programme Supersede Lorentzs .1.' = 'Why Did Einsteins Programme Supersede Lorentz\'s (1)',
        'Modern Moral-Philosophy' = 'Modern Moral Philosophy',
        'Thank Goodness Thats Over' = 'Thank Goodness That\'s Over',
        'Mctaggarts' = 'McTaggart\'s',
        'Must the Scientist Make Value Judgments' = 'Must the Scientist Make Value Judgments?',
        'Anselm Ontological Arguments' = 'Anselm\'s Ontological Arguments',
        'Godel' = 'Gödel',
        'About .1.' = 'About',
        'Aint' = 'Ain\'t',
        'Inference or Performance' = 'Inference or Performance?',
        'Can the Will Be Caused' = 'Can the Will Be Caused?',
        'Is Introspective Knowledge Incorrigible' = 'Is Introspective Knowledge Incorrigible?',
        'Kants Ethics' = 'Kant\'s Ethics',
        'The Effect of Essentialism on Taxonomy - 2000 Years of Stasis .1.' = 'The Effect of Essentialism on Taxonomy—2000 Years of Stasis (1)',
        'Argument for Identity Theory' = 'An Argument for the Identity Theory',
        'Rigorous Proof of Determinism Derived From Special Theory of Relativity' = 'A Rigorous Proof of Determinism Derived from the Special Theory of Relativity',
        'Utterers Meaning' = 'Utterer\'s Meaning',
        'Logic of Inexact Concepts' = 'The Logic of Inexact Concepts',
        'Reasons for Indeterminacy of Translation' = 'On the Reasons for Indeterminacy of Translation',
        'Defense of Abortion' = 'A Defense of Abortion',
        'Semantics of Entailment .2.' = 'The Semantics of Entailment — II',
        'Liar Paradox' = 'The Liar Paradox',
        'Functional-Analysis' = 'Functional Analysis',
        'Pro-Sentential Theory of Truth' = 'A Pro-Sentential Theory of Truth',
        'Schizophrenia of Modern Ethical Theories' = 'The Schizophrenia of Modern Ethical Theories',
        'Semantics of Singular Terms' = 'The Semantics of Singular Terms',
        'Should We Attempt To Justify Induction' = 'Should We Attempt to Justify Induction?',
        'Wittgenstein Philosophy of Mathematics' = 'Wittgenstein\'s Philosophy of Mathematics',
        'Plato Republic' = 'Plato\'s Republic',
        'Nonaesthetic' = 'Non-Aesthetic',
        'Psycho-Physical Identity Theory' = 'On the Psycho-Physical Identity Theory',
        'Causal Theory of Knowing' = 'A Causal Theory of Knowing',
        'Analysis of Factual Knowledge' = 'An Analysis of Factual Knowledge',
        'Should Numbers Count' = 'Should the Numbers Count?',
        'Modus-Ponens' = 'Modus Ponens',
        'Equal-Opportunity for Welfare' = 'Equal Opportunity for Welfare',
        'Conceptual Analysts Defense' = 'Conceptual Analyst\'s Defense',
        'Antifoundationalism' = 'Anti-foundationalism',
        'Decision-Theoretic' = 'Decision-theoretic',
        'Guide to Possibility' = 'Guide to Possibility?',
        'Less Ad-Hoc Theories' = 'Less _Ad Hoc_ Theories',
        'Things Arent Equal' = 'Things Aren\'t Equal',
        'Four Dimensionalism (Persistence Through Time, Doctrine of Temporal Parts, Perdurance)' = 'Four Dimensionalism',
        'the \"Space of Reasons\"' = 'the \'Space of Reasons\'',
        'Blame? a Paradigm' = 'Blame? A Paradigm',
        'Microbiomes? a Comparison' = 'Microbiomes? A Comparison',
        ' - ' = ': ',
        ': a' = ': A',
        ': the' = ': The',
        ': on' = ': On',
        'Action: Problem' = 'Action: The Problem'
      )
    )
  )
  )  |>
  mutate(short_auth = case_when(
    art_title == "Is Conceivability A Guide To Possibility" ~ "Yablo, Conceivability",
    art_title == "Paradox Without Self-Reference" ~ "Yablo, Paradox",
    art_title == "What is Equality? Part 1: Equality of Welfare" ~ "Dworkin Pt. 1",
    art_title == "What is Equality? Part 2: Equality of Resources" ~ "Dworkin Pt. 2",
    art_title == "In Defense Of Proper Functions" ~ "Millikan, Proper Function",
    art_title == "Biosemantics" ~ "Millikan, Biosemantics",
    art_title == "Concepts Of Supervenience" ~ "Kim, Concepts",
    art_title == "Epiphenomenal And Supervenient Causation" ~ "Kim, Epiphenomenal",
    art_title == "Probabilities Of Conditionals And Conditional Probabilities" ~ "Lewis, Probabilities",
    art_title == "Paradoxes Of Time Travel" ~ "Lewis, Time Travel",
    art_title == "Attitudes De-Dicto And De-Se" ~ "Lewis, De Se",
    art_title == "Counterfactual Dependence And Times Arrow" ~ "Lewis, Time\'s Arrow",
    art_title == "Scorekeeping In A Language Game" ~ "Lewis, Scorekeeping",
    art_title == "Functions As Selected Effects: The Conceptual Analysts Defense" ~ "Neander, Selected",
    art_title == "The Teleological Notion Of Function" ~ "Neander, Teleological",
#    art_title == "A Causal Calculus Part 1" ~ "Good Pt. 1",
#    art_title == "A Causal Calculus Part 2" ~ "Good Pt. 2",
    art_title == "Truth and Meaning" ~ "Davidson, Truth",
    art_title == "Causal Relations" ~ "Davidson, Causal",
    art_title == "Possibility" ~ "Hacking, Possibility",
    art_title == "Slightly More Realistic Personal Probability" ~ "Hacking, Probability",
    art_title == "Counterparts Of Persons And Their Bodies" ~ "Lewis, Counterparts",
    art_title == "Immodest Inductive Methods" ~ "Lewis, Inductive",
    art_title == "A Prolegomenon To Meinongian Semantics" ~ "T Parsons",
    art_title == "Liar Paradox" ~ "C Parsons",
    short_auth == "Rabinowicz and Ronnow-Rasmussen" ~ "R and R-R",
    short_auth == "Inwagen" ~ "van Inwagen",
    short_auth == "Fraassen" ~ "van Fraassen",
    TRUE ~ short_auth
  )) |>
  mutate(graph_cite = paste0(
    graph_auth,
    ", \"",
    art_title,
    ",\""
  ))

philo_bib_fix_4 <- philo_bib_fix_3 |>
  ungroup() |>
  mutate(end_of_longcite = str_sub(longcite, str_length(longcite)-35, str_length(longcite)-5)) |>
  mutate(end_of_longcite = str_replace_all(end_of_longcite, "[a-z]", "")) |>
  mutate(end_of_longcite = str_replace_all(end_of_longcite, "[A-Z]", "")) |>
  mutate(end_of_longcite = str_replace_all(end_of_longcite, "[,.?]", "")) |>
  mutate(end_of_longcite = str_replace_all(end_of_longcite, "^:", "")) |>
  mutate(end_of_longcite = str_squish(end_of_longcite)) |>
  mutate(end_of_longcite = str_replace_all(end_of_longcite, "^:", "")) |>
  mutate(end_of_longcite = str_replace_all(end_of_longcite, "^\\)", "")) |>
  mutate(end_of_longcite = str_replace_all(end_of_longcite, "^\\&", "")) |>
  mutate(end_of_longcite = str_replace_all(end_of_longcite, "-$", "")) |>
  mutate(end_of_longcite = str_replace_all(end_of_longcite, " 1$", "")) |>
  mutate(end_of_longcite = str_replace_all(end_of_longcite, " 2$", "")) |>
  mutate(end_of_longcite = str_replace_all(end_of_longcite, " 4$", "")) |>
  mutate(end_of_longcite = str_replace_all(end_of_longcite, " 17$", "")) |>
  mutate(end_of_longcite = str_squish(end_of_longcite)) |>
  mutate(end_of_longcite = case_when(
    art_title == "Disagreement, Question-Begging And Epistemic Self-Criticism" ~ "11 (6): 1-22",
    art_title == "Grounding Explanations" ~ "13 (7): 1-26",
    art_title == "The Possibility Of Physicalism" ~ "111 (9-10): 557-592",
    art_title == "Unified Foundations For Essence And Ground" ~ "1 (2): 296-311",
    art_title == "Probability and Conditionals" ~ "37 (1): 64-80",
    id == "WOS:000366669500008" ~ "58 (7-8): 828-874",
    id == "WOS:A1961CGR7700001" ~ "70 (1): 1-24",
    id == "WOS:A1971J127200004" ~ "38 (1): 54-63",
    TRUE ~ end_of_longcite)
  ) |>
  mutate(end_of_longcite = str_replace_all(end_of_longcite, " ", " ")) |>
  select(end_of_longcite, everything()) |>
  ungroup()

philo_bib_fix_5 <- philo_bib_fix_4 |>
  mutate(full_cite = paste0(graph_auth, 
                            " (",
                            year,
                            ") \"",
                            art_title,
                            ",\" _",
                            journal,
                            "_ ",
                            end_of_longcite,
                            ".")) |>
  mutate(cite_without_year = paste0(graph_auth, 
                                    " \"",
                                    art_title,
                                    ",\" _",
                                    journal,
                                    "_ ",
                                    end_of_longcite))

philo_bib_fix_6 <- philo_bib_fix_5 |>
  select(id, journal, year, art_title, end_of_longcite, auth, firstauth, graph_auth, short_auth, graph_cite, full_cite, cite_without_year, shortcite) |>
  mutate(journal = case_when(
    journal == "Philosophers Imprint" ~ "Philosophers' Imprint",
    TRUE ~ journal
  ))


philo_bib_fix <- philo_bib_fix_6
save(philo_bib_fix, file = "philo_bib_fix.RData")
save(philo_bib_fix_6, file = "philo_bib_fix_without_jphil.RData")

load("philo_cite.RData")
philo_cite_fix <- philo_cite |>
  filter(id %in% philo_bib_fix$id) |>
  filter(refs %in% philo_bib_fix$id)
save(philo_cite_fix, file = "philo_cite_fix.RData")
