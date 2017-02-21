
I implemented two solutions in Haskell, a simpler one based on a k Nearest Neighbors approach using Jaccard distance, and a more involved low-rank matrix factorization (MF) approach based on the method of Hu, Koren, and Volinsky in [Collaborative Filtering for Implicit Feedback Datasets](http://yifanhu.net/PUB/cf.pdf). The code is separately documented, and can be found in the included project (the `test` directory is a good place to start). It represents my submission in terms of showcasing formal development skills / coding style best practices.

The `data` folder contains the recommedation files for all three solutions (`data/recommend_1-KNN.csv`, `data/recommend_1-SGD.csv` and `data/recommend_1-ALS.csv`).


For many real-world user-item interactions there are no explicit rating data available.  However, there is often nontrivial information about the interactions, e.g. clicks, listens/watches, purchases, etc.  Such indirect "ratings" information about user-item interactions is known as implicit feedback. Modeling implicit feedback is a difficult but important problem. There are several ways to use the ALS matrix factorization to approach such a model.

The basic approach is to forget about modeling the implicit feedback directly. Rather, we want to understand whether user u has a preference or not for item i using a simple boolean variable which we denote by $p_{ui}.$ The number of purchases will be interpreted as our confidence in our model.
Following the fundamental idea of matrix factorization models, we try to find a user vector $x_u$ for each user u and an item vector $y_i$ for each item i so that 
$$p_{ui} \sim x^T_u y_i.$$

It is important to note that we never actually observe $p_{ui}$! (This is a very different situation than the explicit feedack models, as discussed above, where $r_{ui}$ is the observer rating.) Let's assume the our implicit feedback is a positive integer (number of clicks, number of views, number of listens, etc). That is, 
$$r_{ui} = \text{# of times user }\mathtt{u}\text{ interacted with item }\mathtt{i}.$$

How do we go about finding the vectors $x_u$ and $y_i$ given some implicit feedback $\{r_{ui}\}$? If a user has interacted with an item, we have reason to believe that $p_{ui}=1$. The more they have interacted with that item, the stronger our belief in their preference.To define our model, set

$$ p_{ui} = \begin{cases} 1 & \text{if }r_{ui}>0\\ 0 & \text{if }r_{ui}=0.\end{cases}$$

We try to minimize the following cost function:

$$ C_\text{implicit} := \sum_{u,i\in\text{observed interactions}} c_{ui}\left(p_{ui} - x^T_u y_i \right)^2 + \lambda \left(\sum_u \|x_u\|^2 + \sum_i \|y_i\|^2\right),$$

where $c_{ui}$ is our <strong>confidence</strong> in $p_{ui}.$ That is, the more a user has interacted with an item, the more we penalize our model for incorrectly predicting $p_{ui}.$

If a user has never interacted with an item, it is possible that $p_{ui}=1$ and the user has just never encountered the item. It is also possible they are actively avoiding the item. To deal with this ambiguity, it is common to define the confidence by

$$c_{ui}:=1 + \alpha r_{ui},$$

where $\alpha$ is a parameter of the model that needs to be tuned to the dataset. There is some empirical evidence that setting it to the sparsity ratio (the ratio of nonzero entries to zero entries) works well; missing entries are often interpreted as slightly negative, and so one could interpret $\alpha$ as balancing positive and negative interactions. Another possibility that works well (and makes the model more robust against power users -- those users who interact with items orders of magnitude more often than the average user), which is also mentioned in the paper, is to set

$$c_{ui} = 1 + \alpha\log(1+r_{ui}/\epsilon),$$

where $\epsilon$ is yet another data-dependent parameter of the model.