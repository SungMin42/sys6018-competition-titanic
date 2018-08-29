prediction = predict(glm_5, df.test)
prediction = ifelse(prediction > 0.5, 1, 0)
prediction = ifelse(is.na(prediction), 0, prediction)
prediction

solution = data_frame('PassengerId' = passenger.id, 'Survived' = prediction)
write_csv(solution, 'solution2.csv')

