#!/usr/bin/env groovy

void colorize(Closure steps) {
  wrap($class: 'AnsiColorBuildWrapper', colorMapName: 'xterm') {
    steps()
  }
}

node {
  timestamps {
    colorize {
      stage('Checkout') {
        checkout scm
      }
      stage('Build') {
      	sh 'make docker-image'
      }
      if (env.BRANCH_NAME == 'master') {
        stage('Push image') {
          sh 'make docker-push'
	}
      }
    }
  }
}

