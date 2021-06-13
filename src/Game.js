import React from 'react';
import PengineClient from './PengineClient';
import Board from './Board';

class Game extends React.Component {

  pengine;

  constructor(props) {
    super(props);
    this.restart(true)
  }



  handlePengineCreate() {
    const queryS = 'init(PistasFilas, PistasColumns, Grilla)';
    this.pengine.query(queryS, (success, response) => {
      if (success) {
        this.setState({
          grid: response['Grilla'],
          rowClues: response['PistasFilas'],
          colClues: response['PistasColumns'],
          satisfaccionFil: Array(response['PistasFilas'].length).fill(false), //inicializamos la fila para verificar en falso
          satisfaccionCol: Array(response['PistasColumns'].length).fill(false) //inicializamos la columna para verificar en falso
        });
        this.cargarGrillaRes();
      }
      
      //this.mostrarSol();
    });
    
  }

  cargarGrillaRes(){
    
    const PF= this.state.rowClues.length;
    const PC= this.state.colClues.length;
    const rowClue = JSON.stringify(this.state.rowClues);
    const colClue = JSON.stringify(this.state.colClues);
    const queryX = 'resolverNonograma('+ PF + ',' + PC + ', ' + rowClue + ', ' + colClue +', GrillaSo)';
    this.pengine.query(queryX, (success, response) => {
      if (success) {
        let GrillaSol = response['GrillaSo'];
        this.setState({
          gridSol: GrillaSol
        });
      } else {
        this.setState({
          waiting: false
        });
      }
    });

  }

  handleClick(i, j) {
    // No action on click if we are waiting.
    if (this.state.waiting) {
      return;
    }
    // Build Prolog query to make the move, which will look as follows:
    // put("#",[0,1],[], [],[["X",_,_,_,_],["X",_,"X",_,_],["X",_,_,_,_],["#","#","#",_,_],[_,_,"#","#","#"]], GrillaRes, FilaSat, ColSat)
    const squaresS = JSON.stringify(this.state.grid).replaceAll('"_"', "_"); // Remove quotes for variables.
    const modo = this.state.modoBoton;
    const rowClue = JSON.stringify(this.state.rowClues);
    const colClue = JSON.stringify(this.state.colClues);
    const queryS = 'put("' + modo + '", [' + i + ',' + j + ']' + ', ' + rowClue + ', ' + colClue + ',' + squaresS + ', GrillaRes, FilaSat, ColSat)';
    this.setState({
      waiting: true
    });

    this.pengine.query(queryS, (success, response) => {
      if (success) {
        let newGrid = response['GrillaRes'];
        let satisfaceFil = response['FilaSat'];
        let satisfaceCol = response['ColSat'];
        this.setState({
          grid: newGrid
        });
        this.confirmarFilaSatisfecha(i, satisfaceFil === 1);
        this.confirmarColSatisfecha(j, satisfaceCol === 1);
      } else {
        this.setState({
          waiting: false
        });
      }
    });
  }





  mostrarSol(){
    const G= this.state.gridSol;

    for(let i=0;i<G.length;i++)
      for(let j=0;j<G[0].length;j++)
          console.log(JSON.stringify(G[i][j]));
  }
  /**
   * Verifica si la columna con el indice index verifica la propiedad.
   * @param {*} index es el indice de la columna que deseamos verificar 
   * @param {*} satisface indica si se satisfacio la columna 
   */
  confirmarColSatisfecha(index, satisface) {
    let satisfaccionCol = [...this.state.satisfaccionCol];
    satisfaccionCol[index] = satisface;
    this.setState({ satisfaccionCol, waiting: false });
  }

  /**
   * Verifica si la fila con el indice index verifica la propiedad.
   * @param {*} index es el indice de la fila que deseamos verificar 
   * @param {*} satisface indica si se satisfacio la fila
   */

  confirmarFilaSatisfecha(index, satisface) {
    let satisfaccionFil = [...this.state.satisfaccionFil];
    satisfaccionFil[index] = satisface;
    this.setState({ satisfaccionFil, waiting: false });
  }
  /**
   * Cambia el estado del boton, alternando entre el # y la X según cual sea su estado actual.
   */
  cambiar() {
    if (this.state.modoBoton === "#") {
      this.setState({ modoBoton: "X" })
    }
    else
      this.setState({ modoBoton: "#" })
  }

  resolver(){
    if(this.state.modoResuelto==false)
      this.setState.modoResuelto=true;
    else
      this.setState.modoResuelto=false;
    const grilla= this.state.grid;
    console.log(JSON.stringify("Grilla: "+this.state.gridSol));
    for(let i=0;i<grilla.length;i++)
      for(let j=0;i<grilla[0].length;j++){
      
      }
        
  }
  

  ayuda(){
    
  }

  abrirManual() {
    window.open("https://es.puzzle-nonograms.com/faq.php", "Manual de usuario / Tutorial", "width=800, height=700")
  }
  irGithub() {
    window.open("https://github.com/LeooRodriguez/Nonogram", "GitHub", "width=800, height=700")
  }


  render() {
    if (this.state.grid === null) {
      return null;
    }



    let { satisfaccionFil, satisfaccionCol } = this.state;

    if (satisfaccionFil.every(a => a) && satisfaccionCol.every(a => a)) {//Si todas las filas y columnas de la grilla son true --> ganaste el juego.
      return <div>
        <div className={"cartelGanaste"}>¡FELICIDADES,HAS GANADO!</div>

        <botonesGenerales className={"botonDeReset"} onClick={() => this.restart()}>
          Pulse para comenzar otra partida
        </botonesGenerales>

        <botonesGenerales className={"botonDeGit"} onClick={() => this.irGithub()}>
          Código del nonograma (GitHub)
        </botonesGenerales>
      </div>
    }


    return (
      <div className="game">
        <Board
          grid={this.state.grid}
          rowClues={this.state.rowClues}
          colClues={this.state.colClues}
          onClick={(i, j) => this.handleClick(i, j)}
          satisfaccionFil={this.state.satisfaccionFil}
          satisfaccionCol={this.state.satisfaccionCol}
        />
        <div>
        <button className="box clue nsat" onClick={() => this.cambiar()} >
          {this.state.modoBoton}
        </button>
          <div className={"cartelNombre"}>Nonograma
        </div>
            <div className={"Estado"}>
        </div>
        <botonesGenerales className={"botonReglas"} onClick={() => this.abrirManual()}>
          Reglas
        </botonesGenerales>
        <botonesGenerales className={"botonDeReinicio"} onClick={() => this.restart()}>
          Reiniciar
        </botonesGenerales>
        <botonesGenerales className={"botonDeResolver"} onClick={() => this.resolver()}>
          Resolver
        </botonesGenerales>
        
        <div class = "ayuda" >
          <input type = "checkbox" name="boton" value ={this.state.modoAyudita} onClick={()=>this.ayuda()}/>
          <label for = "boton"></label>
        </div>

        <div class = "solucion" >
          <input type = "checkbox" name="boton" value ={this.state.modoResuelto} onClick={()=>this.resolver()}/>
          <label for = "boton"></label>
        </div>
        
      </div>
      <div>
      <botonesGenerales className={"botonAyudita"} onClick={() => this.ayuda()}>
          Ayudita 
        </botonesGenerales>
        
        </div>
      </div>

    );
  }

  
  restart(isFirstTime = false) {
    this.state = {
      grid: null,
      rowClues: null,
      colClues: null,
      waiting: false,
      satisfaccionFil: [],//Guarda verdaderos o falsos dependiendo si la filas cumple las propiedades del nonograma. 
      satisfaccionCol: [],//Guarda verdaderos o falsos dependiendo si la columnas cumple las propiedades del nonograma.
      modoBoton: "#",//Verifica en que estado esta el botón ( # ó X).
      modoResuelto: false,
      modoAyudita: false,
    };
    this.handleClick = this.handleClick.bind(this);
    this.handlePengineCreate = this.handlePengineCreate.bind(this);
    if (isFirstTime) {
      this.pengine = new PengineClient(this.handlePengineCreate);
    }
    else {
      this.handlePengineCreate()//Inicia una nueva partida ya mismo.
    }
  }
}


















export default Game;


