import matplotlib.pyplot as plt

class Evaluation:
  def __init__(self, true_classes, predicted_classes, neg=0, pos=1):
    self.true_classes = true_classes
    self.predicted_classes = predicted_classes
    self.n = len(true_classes)
    self.neg = neg
    self.pos = pos

  def accuracy(self):
    accuracy = 0
    for c in range(self.n):
      if self.true_classes[c] == self.predicted_classes[c]:
        accuracy += 1
    return accuracy / self.n

  def precision(self):
    true_pos = 0
    false_pos = 0
    for c in range(self.n):
      if self.true_classes[c] == self.predicted_classes[c] == self.pos:
        true_pos += 1
      if self.true_classes[c] == self.neg != self.predicted_classes[c]:
        false_pos += 1
    return true_pos / (true_pos + false_pos)

  def recall(self):
    true_pos = 0
    false_neg = 0
    for c in range(self.n):
      if self.true_classes[c] == self.predicted_classes[c] == self.pos:
        true_pos += 1
      if self.true_classes[c] == self.pos != self.predicted_classes[c]:
        false_neg += 1
    return true_pos / (true_pos + false_neg)
 
  def specificity(self):
    true_neg = 0
    false_pos = 0
    for c in range(self.n):
      if self.true_classes[c] == self.predicted_classes[c] == self.neg:
        true_neg += 1
      if self.true_classes[c] == self.neg != self.predicted_classes[c]:
        false_pos += 1
    return true_neg / (true_neg + false_pos)

  def plot_metrics(self):
    acc = self.accuracy()
    prec = self.precision()
    rec = self.recall()
    spec = self.specificity()
    plt.plot(0, acc, "ro", label="Accuracy")
    plt.plot(1, prec, "bo", label="Precision")
    plt.plot(2, rec, "go", label="Recall")
    plt.plot(3, spec, "yo", label="Specificity")
    plt.legend()

