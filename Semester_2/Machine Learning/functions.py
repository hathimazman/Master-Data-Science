import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
from sklearn.preprocessing import LabelEncoder
from sklearn.linear_model import LogisticRegression
from sklearn.model_selection import KFold, cross_val_score, cross_val_predict
from sklearn.metrics import (
    confusion_matrix, f1_score, classification_report, 
    accuracy_score, precision_score, recall_score
)
from itertools import combinations
import warnings
warnings.filterwarnings('ignore')

class FeatureSelector:
    def __init__(self, X, y, cv_folds=5, random_state=42, n_jobs=-1,
                 score_metric='f1_weighted', class_names=None):
        """
        Feature selection for classification problems with cross-validation and comprehensive metrics.
        
        Parameters:
        -----------
        X : pandas DataFrame
            The feature matrix
        y : array-like
            The target variable (encoded as integers)
        cv_folds : int, optional
            Number of folds for cross-validation
        random_state : int, optional
            Random seed for reproducibility
        n_jobs : int, optional
            Number of parallel jobs for cross-validation
        score_metric : str, optional
            Metric to use for selection ('f1_weighted', 'accuracy', etc.)
        class_names : list, optional
            List of class names corresponding to the encoded y values
        """
        self.X = X
        self.y = y
        self.cv_folds = cv_folds
        self.random_state = random_state
        self.n_jobs = n_jobs
        self.score_metric = score_metric
        self.features = list(X.columns)
        self.class_names = class_names
        self.n_classes = len(np.unique(y))
        self.results_history = {}
        
        if self.class_names is None:
            self.class_names = [f"Class_{i}" for i in range(self.n_classes)]
    
    def _evaluate_model(self, features):
        """
        Evaluate a model with the given features using cross-validation.
        
        Parameters:
        -----------
        features : list
            List of feature names to include in the model
            
        Returns:
        --------
        mean_score : float
            Mean score (f1, accuracy, etc.) from cross-validation
        all_metrics : dict
            Dictionary containing detailed metrics
        """
        if not features:
            return 0.0, {"f1_weighted": 0.0, "accuracy": 0.0}
        
        # Create feature subset
        X_subset = self.X[features]
        
        # Configure cross-validation
        kf = KFold(n_splits=self.cv_folds, shuffle=True, random_state=self.random_state)
        
        # Create the model (LogisticRegression for multi-class problems)
        model = LogisticRegression(random_state=self.random_state, 
                                   max_iter=10000, 
                                   n_jobs=self.n_jobs,
                                   multi_class='multinomial')
        
        # Cross-validation scores
        cv_scores = cross_val_score(model, X_subset, self.y, 
                                   cv=kf, 
                                   scoring=self.score_metric)
        
        # Get predictions for confusion matrix
        y_pred = cross_val_predict(model, X_subset, self.y, cv=kf)
        
        # Calculate various metrics
        metrics = {
            "f1_weighted": f1_score(self.y, y_pred, average='weighted'),
            "f1_macro": f1_score(self.y, y_pred, average='macro'),
            "f1_micro": f1_score(self.y, y_pred, average='micro'),
            "accuracy": accuracy_score(self.y, y_pred),
            "precision_weighted": precision_score(self.y, y_pred, average='weighted'),
            "recall_weighted": recall_score(self.y, y_pred, average='weighted'),
            "confusion_matrix": confusion_matrix(self.y, y_pred),
            "cv_scores": cv_scores,
            "cv_mean_score": np.mean(cv_scores),
            "cv_std_score": np.std(cv_scores),
            "n_features": len(features),
            "features": features,
        }
        
        return metrics[f"cv_mean_score"], metrics
    
    def forward_selection(self, max_features=None, save_all_models=True):
        """
        Forward selection: starts with no features and adds them one by one.
        
        Parameters:
        -----------
        max_features : int, optional
            Maximum number of features to include
        save_all_models : bool, optional
            Whether to save metrics for all intermediate models
            
        Returns:
        --------
        selected_features : list
            List of selected feature names in order of selection
        best_score : float
            The best score achieved
        """
        # Initialize with empty model
        selected_features = []
        remaining_features = self.features.copy()
        best_score = 0  # For metrics like F1, accuracy, higher is better
        
        if max_features is None:
            max_features = len(self.features)
        
        results = []
        
        print(f"\nStarting Forward Selection (max {max_features} features)...")
        
        for i in range(min(max_features, len(self.features))):
            best_feature = None
            current_best_score = best_score
            
            for feature in remaining_features:
                # Current feature set plus candidate feature
                current_features = selected_features + [feature]
                
                # Evaluate model with current features
                score, metrics = self._evaluate_model(current_features)
                
                # Update if better
                if score > current_best_score:
                    current_best_score = score
                    best_feature = feature
                    best_metrics = metrics
            
            # If no improvement or all features used
            if best_feature is None:
                print(f"Forward selection stopped at {i} features. No further improvement.")
                break
            
            # Add best feature
            selected_features.append(best_feature)
            remaining_features.remove(best_feature)
            best_score = current_best_score
            
            result_key = f"forward_{len(selected_features)}"
            metrics_summary = {
                "step": i + 1,
                "n_features": len(selected_features),
                "added_feature": best_feature,
                "selected_features": selected_features.copy(),
                "score": best_score,
                "metrics": best_metrics
            }
            
            results.append(metrics_summary)
            if save_all_models:
                self.results_history[result_key] = metrics_summary
            
            print(f"Step {i+1}: Added '{best_feature}', {self.score_metric} = {best_score:.4f}")
        
        # Store final model
        self.results_history["forward_final"] = results[-1]
        
        return results
    
    def backward_selection(self, min_features=1, save_all_models=True):
        """
        Backward selection: starts with all features and removes them one by one.
        
        Parameters:
        -----------
        min_features : int, optional
            Minimum number of features to keep
        save_all_models : bool, optional
            Whether to save metrics for all intermediate models
            
        Returns:
        --------
        selected_features : list
            List of selected feature names
        best_score : float
            The best score achieved
        """
        # Start with all features
        selected_features = self.features.copy()
        
        # Evaluate initial model
        best_score, best_metrics = self._evaluate_model(selected_features)
        
        results = []
        current_step = 0
        initial_metrics = {
            "step": current_step,
            "n_features": len(selected_features),
            "removed_feature": None,
            "selected_features": selected_features.copy(),
            "score": best_score,
            "metrics": best_metrics
        }
        results.append(initial_metrics)
        
        if save_all_models:
            self.results_history[f"backward_{len(selected_features)}"] = initial_metrics
        
        print(f"\nStarting Backward Selection (min {min_features} features)...")
        print(f"Initial model with {len(selected_features)} features: {self.score_metric} = {best_score:.4f}")
        
        while len(selected_features) > min_features:
            current_step += 1
            worst_feature = None
            best_score_without_feature = 0
            
            for feature in selected_features:
                # Create model without current feature
                current_features = [f for f in selected_features if f != feature]
                
                # Evaluate model without this feature
                score, metrics = self._evaluate_model(current_features)
                
                # If removing this feature improves or maintains performance
                if score > best_score_without_feature:
                    best_score_without_feature = score
                    worst_feature = feature
                    candidate_metrics = metrics
            
            # If removing the worst feature improves (or minimally decreases) the score
            if best_score_without_feature >= (best_score - 0.001):  # Small tolerance to allow minimal decreases
                selected_features.remove(worst_feature)
                best_score = best_score_without_feature
                best_metrics = candidate_metrics
                
                result_key = f"backward_{len(selected_features)}"
                metrics_summary = {
                    "step": current_step,
                    "n_features": len(selected_features),
                    "removed_feature": worst_feature,
                    "selected_features": selected_features.copy(),
                    "score": best_score,
                    "metrics": best_metrics
                }
                
                results.append(metrics_summary)
                if save_all_models:
                    self.results_history[result_key] = metrics_summary
                
                print(f"Step {current_step}: Removed '{worst_feature}', {self.score_metric} = {best_score:.4f}")
            else:
                print(f"Backward selection stopped at {len(selected_features)} features. No further improvement.")
                break
        
        # Store final model
        self.results_history["backward_final"] = results[-1]
        
        return results
    
    def stepwise_selection(self, max_features=None, min_features=1, save_all_models=True):
        """
        Stepwise selection: combines forward and backward selection.
        
        Parameters:
        -----------
        max_features : int, optional
            Maximum number of features to include
        min_features : int, optional
            Minimum number of features to keep
        save_all_models : bool, optional
            Whether to save metrics for all intermediate models
            
        Returns:
        --------
        selected_features : list
            List of selected feature names
        best_score : float
            The best score achieved
        """
        # Initialize with empty model
        selected_features = []
        remaining_features = self.features.copy()
        best_score = 0  # For metrics like F1, accuracy, higher is better
        
        if max_features is None:
            max_features = len(self.features)
        
        results = []
        current_step = 0
        
        print(f"\nStarting Stepwise Selection (min {min_features}, max {max_features} features)...")
        
        while len(selected_features) < max_features and remaining_features:
            current_step += 1
            # Forward step
            best_feature_to_add = None
            best_score_add = best_score
            
            for feature in remaining_features:
                # Create model with current selected features plus the candidate feature
                current_features = selected_features + [feature]
                
                # Evaluate model with current features
                score, metrics = self._evaluate_model(current_features)
                
                # Update if better
                if score > best_score_add:
                    best_score_add = score
                    best_feature_to_add = feature
                    candidate_metrics = metrics
            
            # If adding improves the score, add it
            if best_score_add > best_score and best_feature_to_add is not None:
                selected_features.append(best_feature_to_add)
                remaining_features.remove(best_feature_to_add)
                best_score = best_score_add
                
                result_key = f"stepwise_{current_step}"
                metrics_summary = {
                    "step": current_step,
                    "action": "added",
                    "feature": best_feature_to_add,
                    "n_features": len(selected_features),
                    "selected_features": selected_features.copy(),
                    "score": best_score,
                    "metrics": candidate_metrics
                }
                
                results.append(metrics_summary)
                if save_all_models:
                    self.results_history[result_key] = metrics_summary
                
                print(f"Step {current_step}: Added '{best_feature_to_add}', {self.score_metric} = {best_score:.4f}")
                
                # Backward step (only if we have more than min_features)
                if len(selected_features) > min_features:
                    worst_feature = None
                    best_score_without_feature = 0
                    
                    for feature in selected_features:
                        # Create model without current feature
                        current_features = [f for f in selected_features if f != feature]
                        
                        # Skip if this would go below min_features
                        if len(current_features) < min_features:
                            continue
                        
                        # Evaluate model without this feature
                        score, metrics = self._evaluate_model(current_features)
                        
                        # If removing improves performance
                        if score > best_score_without_feature:
                            best_score_without_feature = score
                            worst_feature = feature
                            removal_metrics = metrics
                    
                    # If removing a feature improves the score
                    if best_score_without_feature > best_score and worst_feature is not None:
                        current_step += 1
                        selected_features.remove(worst_feature)
                        remaining_features.append(worst_feature)
                        best_score = best_score_without_feature
                        
                        result_key = f"stepwise_{current_step}"
                        metrics_summary = {
                            "step": current_step,
                            "action": "removed",
                            "feature": worst_feature,
                            "n_features": len(selected_features),
                            "selected_features": selected_features.copy(),
                            "score": best_score,
                            "metrics": removal_metrics
                        }
                        
                        results.append(metrics_summary)
                        if save_all_models:
                            self.results_history[result_key] = metrics_summary
                        
                        print(f"Step {current_step}: Removed '{worst_feature}', {self.score_metric} = {best_score:.4f}")
            else:
                print(f"Stepwise selection stopped at {len(selected_features)} features. No further improvement.")
                break
                
        # Store final model
        self.results_history["stepwise_final"] = results[-1]
        
        return results
    
    def all_k_models(self, k_range=None):
        """
        Evaluate models with different numbers of features using forward selection.
        
        Parameters:
        -----------
        k_range : list, optional
            List of k values to evaluate
            
        Returns:
        --------
        results : dict
            Dictionary with results for each k value
        """
        if k_range is None:
            k_range = range(1, len(self.features) + 1)
            
        results = {}
        
        # Get forward selection results
        forward_results = self.forward_selection(max_features=max(k_range))
        
        # Extract results for each k
        for k in k_range:
            if k <= len(forward_results):
                k_model = forward_results[k-1]  # -1 because k starts from 1 but index from 0
                results[k] = {
                    "features": k_model["selected_features"],
                    "score": k_model["score"],
                    "metrics": k_model["metrics"]
                }
        
        return results
            
    def plot_model_comparison(self, title="Feature Selection Methods Comparison"):
        """
        Plot comparison of different feature selection methods.
        """
        # Extract results from history
        methods = ['forward', 'backward', 'stepwise']
        valid_methods = [m for m in methods if f"{m}_final" in self.results_history]
        
        if not valid_methods:
            print("No results to plot. Run selection methods first.")
            return
        
        # Prepare data for plotting
        df_list = []
        
        # Process all models from each method's history
        for method in valid_methods:
            # Get all keys for this method
            method_keys = [k for k in self.results_history.keys() 
                          if k.startswith(method) and k != f"{method}_final"]
            
            for key in method_keys:
                result = self.results_history[key]
                n_features = result["n_features"]
                score = result["score"]
                df_list.append({
                    "Method": method.capitalize(),
                    "Number of Features": n_features,
                    f"{self.score_metric.capitalize()}": score
                })
        
        # Create DataFrame
        df = pd.DataFrame(df_list)
        
        # Create plot
        plt.figure(figsize=(12, 6))
        sns.lineplot(data=df, x="Number of Features", y=f"{self.score_metric.capitalize()}", 
                    hue="Method", marker="o", markersize=8)
        
        plt.title(title, fontsize=15)
        plt.xlabel("Number of Features", fontsize=12)
        plt.ylabel(f"{self.score_metric.capitalize()} Score", fontsize=12)
        plt.grid(True, linestyle='--', alpha=0.7)
        plt.tight_layout()
        
        return plt
        
    def plot_confusion_matrices(self, models=None):
        """
        Plot confusion matrices for selected models.
        
        Parameters:
        -----------
        models : list, optional
            List of model keys to plot (e.g., ["forward_final", "backward_final"])
        """
        if models is None:
            models = [k for k in self.results_history.keys() if k.endswith("_final")]
        
        n_models = len(models)
        if n_models == 0:
            print("No models to plot. Run selection methods first.")
            return
        
        # Create a figure with subplots
        fig, axes = plt.subplots(1, n_models, figsize=(7*n_models, 6))
        if n_models == 1:
            axes = [axes]  # Make it iterable
        
        for i, model_key in enumerate(models):
            if model_key not in self.results_history:
                print(f"Model {model_key} not found in results history.")
                continue
                
            # Get confusion matrix
            cm = self.results_history[model_key]["metrics"]["confusion_matrix"]
            
            # Plot confusion matrix
            sns.heatmap(cm, annot=True, fmt="d", cmap="Blues", ax=axes[i], 
                       xticklabels=self.class_names, yticklabels=self.class_names)
            
            # Labels and title
            axes[i].set_xlabel("Predicted", fontsize=12)
            axes[i].set_ylabel("Actual", fontsize=12)
            
            # Format title
            method = model_key.split("_")[0].capitalize()
            n_features = self.results_history[model_key]["n_features"]
            score = self.results_history[model_key]["score"]
            title = f"{method} Selection\n{n_features} Features, {self.score_metric}={score:.4f}"
            axes[i].set_title(title, fontsize=13)
        
        plt.tight_layout()
        return plt
    
    def print_feature_rankings(self):
        """
        Print feature rankings based on order of selection/elimination.
        """
        methods = ['forward', 'backward', 'stepwise']
        rankings = {}
        
        print("\n===== Feature Rankings =====")
        
        for method in methods:
            if f"{method}_final" not in self.results_history:
                continue
                
            if method == 'forward':
                # For forward, we use the order features were added
                steps = [k for k in self.results_history.keys() 
                        if k.startswith(method) and k != f"{method}_final"]
                ordered_features = []
                
                for step in sorted(steps, key=lambda x: int(x.split('_')[1]) if x.split('_')[1].isdigit() else 0):
                    result = self.results_history[step]
                    feature = result.get("added_feature") or result.get("feature")
                    if feature and feature not in ordered_features:
                        ordered_features.append(feature)
                
                rankings['Forward'] = ordered_features
                
            elif method == 'backward':
                # For backward, we reverse the order of elimination
                steps = [k for k in self.results_history.keys() 
                        if k.startswith(method) and k != f"{method}_final"]
                ordered_features = []
                
                for step in sorted(steps, key=lambda x: int(x.split('_')[1]) if x.split('_')[1].isdigit() else 0,
                                reverse=True):
                    result = self.results_history[step]
                    feature = result.get("removed_feature")
                    if feature and feature not in ordered_features:
                        ordered_features.append(feature)
                
                rankings['Backward'] = ordered_features
                
            elif method == 'stepwise':
                # For stepwise, we track additions and deletions
                steps = sorted([k for k in self.results_history.keys() 
                              if k.startswith(method) and k != f"{method}_final"],
                             key=lambda x: int(x.split('_')[1]) if x.split('_')[1].isdigit() else 0)
                
                # Final selected features in order of importance
                final_features = self.results_history[f"{method}_final"]["selected_features"]
                
                # Added features that remained in the final model
                added_features = []
                for step in steps:
                    result = self.results_history[step]
                    if result["action"] == "added" and result["feature"] in final_features:
                        added_features.append(result["feature"])
                
                rankings['Stepwise'] = added_features
        
        # Print rankings
        max_len = max([len(v) for v in rankings.values()]) if rankings else 0
        
        for method, features in rankings.items():
            print(f"\n{method} Selection Feature Ranking:")
            for i, feature in enumerate(features, 1):
                if i <= max_len:
                    print(f"{i}. {feature}")
        
        return rankings
    
    def get_best_model_per_k(self):
        """
        Find the best model for each number of features.
        
        Returns:
        --------
        best_models : dict
            Dictionary with best model for each k value
        """
        # Group models by number of features
        models_by_k = {}
        
        for key, result in self.results_history.items():
            if key.endswith("_final"):
                continue
                
            k = result["n_features"]
            if k not in models_by_k:
                models_by_k[k] = []
            
            models_by_k[k].append({
                "key": key,
                "score": result["score"],
                "features": result["selected_features"]
            })
        
        # Find best model for each k
        best_models = {}
        for k, models in models_by_k.items():
            best_model = max(models, key=lambda x: x["score"])
            best_models[k] = best_model
        
        return best_models
    
    def print_best_model_summary(self):
        """
        Print summary of the best model from each method.
        """
        methods = ['forward', 'backward', 'stepwise']
        valid_methods = [m for m in methods if f"{m}_final" in self.results_history]
        
        if not valid_methods:
            print("No results to summarize. Run selection methods first.")
            return
        
        print("\n===== Best Models Summary =====")
        
        # Create comparison table
        rows = []
        for method in valid_methods:
            result = self.results_history[f"{method}_final"]
            metrics = result["metrics"]
            
            row = {
                "Method": method.capitalize(),
                "Features": result["n_features"],
                "Selected": ", ".join(result["selected_features"]),
                f"{self.score_metric}": metrics["cv_mean_score"],
                "Accuracy": metrics["accuracy"],
                "F1 (weighted)": metrics["f1_weighted"],
                "Precision": metrics["precision_weighted"],
                "Recall": metrics["recall_weighted"]
            }
            rows.append(row)
        
        # Create and print DataFrame
        df = pd.DataFrame(rows)
        print(df.to_string(index=False))
        
        # Find overall best model
        best_method = max(valid_methods, key=lambda m: self.results_history[f"{m}_final"]["score"])
        best_result = self.results_history[f"{best_method}_final"]
        
        print(f"\nBest overall model: {best_method.capitalize()} Selection")
        print(f"Number of features: {best_result['n_features']}")
        print(f"Features: {best_result['selected_features']}")
        print(f"{self.score_metric}: {best_result['score']:.4f}")
        
        return best_result["selected_features"]

def compare_all_selection_methods(X, y, cv_folds=5, class_names=None, score_metric='f1_weighted'):
    """
    Run and compare all feature selection methods.
    
    Parameters:
    -----------
    X : pandas DataFrame
        The feature matrix
    y : array-like
        The target variable (encoded as integers)
    cv_folds : int, optional
        Number of folds for cross-validation
    class_names : list, optional
        List of class names corresponding to the encoded y values
    score_metric : str, optional
        Metric to use for selection ('f1_weighted', 'accuracy', etc.)
        
    Returns:
    --------
    selector : FeatureSelector
        The feature selector object with results
    """
    # Initialize feature selector
    selector = FeatureSelector(X, y, cv_folds=cv_folds, class_names=class_names, score_metric=score_metric)
    
    # Run forward selection
    selector.forward_selection()
    
    # Run backward selection
    selector.backward_selection()
    
    # Run stepwise selection
    selector.stepwise_selection()
    
    # Print summary
    selector.print_best_model_summary()
    
    # Plot comparison
    selector.plot_model_comparison()
    plt.show()
    
    # Plot confusion matrices
    selector.plot_confusion_matrices()
    plt.show()
    
    # Print feature rankings
    selector.print_feature_rankings()
    
    return selector

# Example usage
if __name__ == "__main__":
    # Generate some example data - replace with your actual data
    from sklearn.datasets import make_classification
    
    # Create sample multiclass data
    X, y = make_classification(
        n_samples=500, 
        n_features=23,  # 23 features as specified
        n_informative=10, 
        n_redundant=5, 
        n_classes=7,  # 7 classes for weight categories
        n_clusters_per_class=1,
        random_state=42
    )
    
    # Convert to DataFrame for easier feature handling
    feature_names = [f'Feature_{i+1}' for i in range(X.shape[1])]
    X_df = pd.DataFrame(X, columns=feature_names)
    
    # Define class names
    weight_categories = [
        'Insufficient_Weight', 'Normal_Weight', 'Overweight_Level_I',
        'Overweight_Level_II', 'Obesity_Type_I', 'Obesity_Type_II', 'Obesity_Type_III'
    ]
    
    # Run feature selection
    selector = compare_all_selection_methods(
        X_df, 
        y, 
        cv_folds=5, 
        class_names=weight_categories,
        score_metric='f1_weighted'
    )
    
    # If you want to use your own data:
    """
    # Load your data
    df = pd.read_csv('your_data.csv')
    
    # Define features and target
    X = df.drop('target_column', axis=1)
    y_labels = df['target_column']
    
    # Encode target if needed
    le = LabelEncoder()
    y = le.fit_transform(y_labels)
    
    # Define weight categories
    weight_categories = [
        'Insufficient_Weight', 'Normal_Weight', 'Overweight_Level_I',
        'Overweight_Level_II', 'Obesity_Type_I', 'Obesity_Type_II', 'Obesity_Type_III'
    ]
    
    # Run selection
    selector = compare_all_selection_methods(
        X, 
        y, 
        cv_folds=5,
        class_names=weight_categories,
        score_metric='f1_weighted'
    )
    """