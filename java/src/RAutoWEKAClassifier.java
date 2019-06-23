package de.wwu.is.RAutoWEKA;

import autoweka.ConfigurationCollection;
import weka.attributeSelection.AttributeSelection;
import weka.classifiers.Classifier;
import weka.classifiers.Evaluation;
import weka.classifiers.meta.AutoWEKAClassifier;
import weka.core.Instances;

import java.util.Arrays;

/**
 * Subclass of AutoWEKAClassifier for making protected fields available to the RAutoWEKA interface (@see weka.classifiers.meta.AutoWEKAClassifier).
 */
public class RAutoWEKAClassifier extends AutoWEKAClassifier {

    /**
     * Getter for the classifier.
     *
     * @return The classifier.
     */
    public Classifier getClassifier() {
        return super.classifier;
    }

    /**
     * Getter for the attribute selection.
     *
     * @return The attribute selection.
     */
    public AttributeSelection getAttributeSelection() {
        return super.as;
    }

    /**
     * Getter for the classifier class.
     *
     * @return The classifier class.
     */
    public String getClassifierClass() {
        return super.classifierClass;
    }

    /**
     * Getter for the classifier arguments.
     *
     * @return The classifier arguments.
     */
    public String getClassifierArgs() {
        return super.classifierArgs != null ? Arrays.toString(super.classifierArgs) : "";
    }

    /**
     * Getter for the attribute search class.
     *
     * @return The attribute search class.
     */
    public String getAttributeSearchClass() {
        return super.attributeSearchClass;
    }

    /**
     * Getter for the attribute search arguments.
     *
     * @return The attribute search arguments.
     */
    public String getAttributeSearchArgs() {
        return super.attributeSearchArgs != null ? Arrays.toString(super.attributeSearchArgs) : "";
    }

    /**
     * Getter for the attribute evaluation class.
     *
     * @return The attribute evaluation class
     */
    public String getAttributeEvalClass() {
        return super.attributeEvalClass;
    }

    /**
     * Getter for the attribute evaluation arguments.
     *
     * @return The attribute evaluation arguments.
     */
    public String getAttributeEvalArgs() {
        return super.attributeEvalArgs != null ? Arrays.toString(super.attributeEvalArgs) : "";
    }

    /**
     * Getter for the temporary run directories used by the experiments.
     *
     * @return The temporary run directories.
     */
    public String[] getTemporaryRunDirectories() {
        return super.msExperimentPaths;
    }

    /**
     * Getter for the best configurations.
     *
     * @return The best configurations as <c>ConfigurationCollection</c>.
     */
    public ConfigurationCollection getBestConfigurations() {
        return super.bestConfigsCollection;
    }

    /**
     * Getter for the estimated metric values of all runs.
     *
     * @return The estimated metric values.
     */
    public double[] getEstimatedMetricValues() {
        return super.estimatedMetricValues;
    }

    /**
     * Getter for the estimated metric value of the best run.
     *
     * @return The estimated metric value.
     */
    public double getEstimatedMetricValue() {
        return super.estimatedMetricValue;
    }

    /**
     * Getter for the evaluation
     *
     * @return The evaluation.
     */
    public Evaluation getEval() {
        return super.eval;
    }

    /**
     * Getter for the final training time.
     *
     * @return The final training time.
     */
    public double getFinalTrainTime() {
        return finalTrainTime;
    }

    /**
     * Getter for the total number of configurations tried.
     *
     * @return The total number of configurations tried.
     */
    public int getTotalTried() {
        return totalTried;
    }

    /**
     * Calculates the class membership for the given test instances.
     * <p>
     * Adapted from RWeka (0.4-40)
     * Authors: Kurt Hornik [aut, cre], Christian Buchta [ctb], Torsten Hothorn [ctb], Alexandros Karatzoglou [ctb], David Meyer [ctb], Achim Zeileis [ctb]
     * Link: https://cran.r-project.org/package=RWeka
     *
     * @param instances The instances to be classified.
     * @return The predicted classes.
     * @throws Exception If the instances could not be classified successfully.
     */
    public double[] classifyInstances(Instances instances) throws Exception {
        if (super.classifier == null) {
            throw new Exception("Auto-WEKA has not been run yet to get a model!");
        }

        int errorCount = 0;
        double[] prediction = new double[instances.numInstances()];

        for (int i = 0; i < instances.numInstances(); i++) {
            try {
                prediction[i] = super.classifyInstance(instances.instance(i));
            } catch (Exception exception) {
                errorCount++;
                prediction[i] = Double.NaN;
            }
        }

        if (errorCount > 0) {
            System.err.println(errorCount + " instances not classified");
        }

        return prediction;
    }

    /**
     * Calculates the class membership probabilities for the given test instances.
     * <p>
     * Adapted from RWeka (0.4-40)
     * Authors: Kurt Hornik [aut, cre], Christian Buchta [ctb], Torsten Hothorn [ctb], Alexandros Karatzoglou [ctb], David Meyer [ctb], Achim Zeileis [ctb]
     * Link: https://cran.r-project.org/package=RWeka
     *
     * @param instances The instances to be classified.
     * @return The predicted class probability distributions in row-major format.
     * @throws Exception If the instances could not be classified successfully.
     */
    public double[] distributionForInstances(Instances instances) throws Exception {
        if (super.classifier == null) {
            throw new Exception("Auto-WEKA has not been run yet to get a model!");
        }

        int k = 0;
        double[] prediction = new double[instances.numInstances() * instances.numClasses()];

        for (int i = 0; i < instances.numInstances(); i++) {
            double[] tmp = super.distributionForInstance(instances.instance(i));
            for (int j = 0; j < instances.numClasses(); j++, k++) {
                prediction[k] = tmp[j];
            }
        }
        return prediction;
    }

}
