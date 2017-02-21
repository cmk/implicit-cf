# Takt Data Science Project

I implemented two solutions in Haskell, a simpler one based on a k Nearest Neighbors approach using Jaccard distance, and a more involved low-rank matrix factorization (MF) approach based on the method of Hu, Koren, and Volinsky in [Collaborative Filtering for Implicit Feedback Datasets](http://yifanhu.net/PUB/cf.pdf) (there is also an excellent exposition by the Activision data science team [here](http://activisiongamescience.github.io/2016/01/11/Implicit-Recommender-Systems-Biased-Matrix-Factorization)). The code can be found in the included project (the `test` directory is a good place to start). It represents my submission in terms of showcasing formal development skills / coding style best practices.

However a stack project is not the ideal format for documenting a thought process, and you have provided me with a juptyer notebook with Python code in it, so I am also including a more typical 'data scientist'-style solution (below) in order to walk through my process using Python. This solution also uses the MF method of Hu, Koren, and Volinsky. The main difference between the Haskell and Python solutions is that I wrote the Haskell one from scratch using a general purpose stochastic gradient descent (SGD) optimizer, and the Python one uses the alternating least squares (ALS) solver in Ben Frederickson's implicit library. ALS is a second-order method (as opposed to GSD) and Ben's code is parallelized, so this notebook was quite a bit faster than my Haskell version. Otherwise the two methods yielded similar results.

The `data` folder contains the recommedation files for all three solutions (`data/recommend_1-KNN.csv`, `data/recommend_1-SGD.csv` and `data/recommend_1-ALS.csv`).

# Haskell Solution

A bit more about the Haskell code. As I mentioned there are two recommenders, the code for which can be found in `src/KNearestNeighbors.hs` and `src/MatrixFactorization.hs`. The MF code also relies on a bias model `src/Bias.hs` to account for user and item biases (see the Activision post above), this part is still experimental and the bias factor is zeroed out at the moment.
 

Both models were scored against holdout transaction data from `data/trx_data.csv` using a rudimentary click-through-rate type scorer in `test/Exec.hs`. The oldout data files were constructed using shell tools (e.g. `sed`,`split` etc). Since my goal here was to demonstrate solid programming skills, I thought this approach sufficient for a baseline comparison (I used more traditional validation techniques and metrics in the Jupyter notebook). Similarly I settled on a simple Jaccard metric rather than some other metric (e.g. Pearson, cosine etc) for the KNN model, and stochastic gradient descent rather than alternating least squares for the MF model. This led to simpler code and I believe demonstrates good applied mathematics (i.e. create baselines using elementary methods).

The main function, along with a few unit tests can be found in `test/Spec.hs`. 